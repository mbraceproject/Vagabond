namespace Nessos.DistribFsi

    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.Tast
    open Microsoft.FSharp.Compiler.Ast

    open Microsoft.FSharp.Control

    open Nessos.DistribFsi
    open Nessos.DistribFsi.Utils
    open Nessos.DistribFsi.Serialization
    open Nessos.DistribFsi.TastAnalyzer
    open Nessos.DistribFsi.AstTransformer

    open System.Collections.Generic
    open System.Reflection
    open System.Text
    open System.Text.RegularExpressions
    open System.IO
    open System

    type internal CompilerMessage =
        | NextInteraction of ParsedInput list * TypedAssembly * AsyncReplyChannel<exn option>
        | RequestCompilation of AsyncReplyChannel<Choice<FsiCompiledAssembly, exn>>

    type internal InteractionCompiler(outDirectory : string, writePdb : bool, fsiDynamicAssemblyName : string) =

        let shellId = Guid.NewGuid ()
        let signature = Guid.toHex shellId
        let getAssemblyName idx = sprintf "FSI_%s_%d" signature idx
        let assemblyRegex = new Regex("^FSI_" + signature + "_[0-9]+$")
        let fsiDynamicAssembly = lazy(
            AppDomain.CurrentDomain.GetAssemblies() 
            |> Array.find(fun a -> a.IsDynamic && a.GetName().Name = fsiDynamicAssemblyName))
    
        // contain declaration introspection data
        let moduleIndex = ref Map.empty<string, FsiCompiledAssembly>
        let declarationIndex = ref Map.empty<DeclarationId, FsiDeclarationInfo>
        let dependencyIndex = ref Map.empty<FsiCompiledAssembly, AssemblyDescriptor list>

        let appendDeclarations (decls : FsiDeclarationInfo seq) =
            declarationIndex := decls |> Seq.map (fun d -> d.Id, d) |> Map.addMany declarationIndex.Value

        let appendModules (assembly : FsiCompiledAssembly) (moduleNames : string seq) =
            moduleIndex := moduleNames |> Seq.map (fun n -> n, assembly) |> Map.addMany moduleIndex.Value

        let appendDependency (asmb : FsiCompiledAssembly) (dependencies : AssemblyDescriptor list) =
            dependencyIndex := dependencyIndex.Value.Add(asmb, dependencies)

        let serializer = 
            new FsiSerializer(
                {
                    FsiAssemblyName = fsiDynamicAssemblyName
                    CompiledAssemblyPattern = assemblyRegex
                    ModuleIndex = mkReadOnly moduleIndex
                })

        let createAssemblyInfo (n : int) =
            let name = getAssemblyName n
            let an = fsiDynamicAssembly.Value.GetName()
            an.Name <- name
            { 
                Name = name
                FullName = an.FullName 
                Location = Path.Combine(outDirectory, name + ".dll")
                PdbLocation = if writePdb then Some <| Path.Combine(outDirectory, name + ".pdb") else None
            }

        let compile (target : FsiCompiledAssembly) (dependencies : AssemblyDescriptor list) (ast : ParsedInput list) =
            let compilerSvc = new SimpleSourceCodeServices.SimpleSourceCodeServices()
            let assemblies = dependencies |> List.map (fun d -> d.Location)
            let errors, id = compilerSvc.Compile(ast, target.Name, target.Location, assemblies, ?pdbFile = target.PdbLocation, executable = false)
            if id = 0 then ()
            else
                raise <| new Exception(sprintf "Compiler error:\n%A" errors)

        // the actual compilation actor
        let compilerActor =
            let rec behaviour n (dependencies : Set<AssemblyDescriptor>)
                                (astPreamble : SynModuleDecl list)
                                (astQueue : ParsedInput list)
                                (previousInteraction : CurrentInteraction option)
                                (inbox : MailboxProcessor<CompilerMessage>) =      
                async {
                    let! message = inbox.Receive()

                    match message with
                    | NextInteraction(inputs, typedAssembly, rc) ->
                        try
                            // handle previous interaction data and add to AST queue
                            let astQueue, astPreamble, dependencies =
                                match previousInteraction with
                                | None -> astQueue, astPreamble, dependencies
                                | Some info ->
                                    // update the moduleIndex *before* PIC; essential for serialization
                                    do appendModules info.CompiledAssembly info.ModuleNames

                                    let updatedInfo, astPreamble, inputs = 
                                        transformAst fsiDynamicAssembly serializer astPreamble info

                                    let dependencies = Set.addMany dependencies info.Dependencies

                                    do appendDeclarations updatedInfo

                                    astQueue @ inputs, astPreamble, dependencies

                            // create assembly info for current interaction
                            let assemblyInfo = createAssemblyInfo (n+1)

                            // gather fsi metadata from typed assembly
                            let tyIdx, declarations = extractFsiMetadata typedAssembly

                            // gather declaration dependencies from fsi metadata
                            let interactionInfo = buildInteractionData assemblyInfo tyIdx declarations inputs

                            do appendDeclarations interactionInfo.DeclarationInfo        

                            // successful, acknowledge to sender
                            do rc.Reply None

                            return! behaviour n dependencies astPreamble astQueue (Some interactionInfo) inbox

                        with e ->

                            rc.Reply <| Some e
                            return! behaviour n dependencies astPreamble astQueue previousInteraction inbox

                    | RequestCompilation rc ->
                        try
                            let assemblyInfo = createAssemblyInfo (n+1)

                            // recursively traverse dependency graph
                            let allDependencies = gatherDependencies dependencyIndex.Value dependencies

                            // compile
                            compile assemblyInfo allDependencies astQueue

                            // update dependency index
                            do appendDependency assemblyInfo allDependencies

                            let dependencies = Set.add (FsiCompiled assemblyInfo) dependencies

                            // reply to sender
                            rc.Reply <| Choice1Of2 assemblyInfo

                            // compilation succeeded; reset the state and increment the counter
                            return! behaviour (n+1) dependencies astPreamble [] previousInteraction inbox

                        with e ->

                            rc.Reply <| Choice2Of2 e
                            return! behaviour n dependencies astPreamble astQueue previousInteraction inbox
                }

            MailboxProcessor.Start(behaviour 0 (set [ StaticAssembly (Assembly.GetExecutingAssembly()) ]) [] [] None)

        member __.EnqueueInteraction (inputs : ParsedInput list, typedAssembly : TypedAssembly) =
            match compilerActor.PostAndReply <| fun rc -> NextInteraction (inputs, typedAssembly, rc) with
            | None -> ()
            | Some e -> raise e

        member __.RequestCompilation () =
            match compilerActor.PostAndReply RequestCompilation with
            | Choice1Of2 assembly -> assembly
            | Choice2Of2 e -> raise e


        member __.TryGetCompiledAssembly (moduleName : string) = moduleIndex.Value.TryFind moduleName
        member __.TryGetDeclarationInfo (id : DeclarationId) = declarationIndex.Value.TryFind id
        member __.GetDependencies (assembly : FsiCompiledAssembly) = dependencyIndex.Value.[assembly]

        member ic.GetCompilerInfo () =
            {
                new InteractionCompilerInfo with
                    member __.ShellId = shellId
                    member __.DynamicAssembly = fsiDynamicAssembly.Value
                    member __.CompiledAssemblyPath = outDirectory

                    member __.Pickler = serializer.Pickler
                    
                    member __.RequestCompilation () = ic.RequestCompilation ()
                    member __.GetDependencies a = ic.GetDependencies a
                    member __.TryGetCompiledAssembly m = ic.TryGetCompiledAssembly m
                    member __.TryGetDeclarationInfo d = ic.TryGetDeclarationInfo d
            }