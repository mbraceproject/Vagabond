namespace Nessos.DistribFsi.Shell

    open System.Collections.Generic
    open System.Reflection
    open System.Text
    open System.Text.RegularExpressions
    open System.IO
    open System

    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.Tast
    open Microsoft.FSharp.Compiler.Ast

    open Microsoft.FSharp.Control

    open FsPickler

    open Nessos.DistribFsi
    open Nessos.DistribFsi.DependencyAnalysis
    open Nessos.DistribFsi.Shell
    open Nessos.DistribFsi.Shell.Utils
    open Nessos.DistribFsi.Shell.Types

    type internal FsiTypeNameConverter (fsiDynamicAssemblyName : string, compiledAsemblyRegex : Regex) =
        let interactionIndex = ref Map.empty<string, AssemblyName>

        // make early binding of pending interactions to moduleIndex
        // this is essential since AST transformations depend on pickling
        // which in turn depends on the moduleIndex for qualified name transformations
        member __.AddPendingInteractionToModuleIndex(state : InteractionCompilerState) =
            match state.PendingInteraction with
            | None -> ()
            | Some pending ->
                interactionIndex := interactionIndex.Value.Add(pending.Name, state.NextCompiledAssembly)

        interface ITypeNameConverter with
            member __.OfSerializedType (typeInfo : TypeInfo) =
                if typeInfo.AssemblyName = fsiDynamicAssemblyName then
                    let id = DeclarationId.Create(typeInfo.Name)
                    match interactionIndex.Value.TryFind id.TopLevelName with
                    | None -> invalidOp "Internal error: attempting to pickle an unregistered interaction"
                    | Some an ->
                        { typeInfo with AssemblyName = an.Name }
                else
                    typeInfo

            member __.ToDeserializedType (typeInfo : TypeInfo) =
                if compiledAsemblyRegex.IsMatch typeInfo.AssemblyName then
                    { typeInfo with AssemblyName = fsiDynamicAssemblyName }
                else
                    typeInfo
            

    type internal InteractionCompiler(outDirectory : string, writePdb : bool, fsiDynamicAssemblyName : string, serializerFactory : ISerializerFactory) =

        static let append (proj : 'V -> 'K) (state : Map<'K,'V>) (values : seq<'V>) =
            values |> Seq.map (fun v -> proj v, v) |> Map.addMany state

        let shellId = Guid.NewGuid ()
        let signature = Guid.toHex shellId
        let getAssemblyName idx = sprintf "FSI_%s_%d" signature idx
        let assemblyRegex = new Regex("^FSI_" + signature + "_[0-9]+$")
        let fsiDynamicAssembly = lazy(
            AppDomain.CurrentDomain.GetAssemblies() 
            |> Array.find(fun a -> a.IsDynamic && a.GetName().Name = fsiDynamicAssemblyName))

        let tyConv = new FsiTypeNameConverter(fsiDynamicAssemblyName, assemblyRegex)
        let serializer = serializerFactory.Create tyConv

        let getNextAssemblyName (i : int) =
            let name = getAssemblyName i
            let an = fsiDynamicAssembly.Value.GetName()
            an.Name <- name
            an

        let compile (state : InteractionCompilerState) =
            if state.AstQueue.IsEmpty then state
            else
                let an = state.NextCompiledAssembly
                let outFile = Path.Combine(outDirectory, an.Name + ".dll")
                let pdbFile = if writePdb then Some <| Path.Combine(outDirectory, an.Name + ".pdb") else None
                let dependencies = state.Dependencies |> Map.toSeq |> Seq.map fst |> Seq.toList

                let compilerSvc = new SimpleSourceCodeServices.SimpleSourceCodeServices()

                let resetCompilerCounters () = globalNng.Reset() ; globalStableNameGenerator.Reset()

                do resetCompilerCounters ()
                let errors, id = compilerSvc.Compile(state.AstQueue, an.Name, outFile, dependencies, ?pdbFile = pdbFile, executable = false)
                do resetCompilerCounters ()

                if id <> 0 then raise <| new Exception(sprintf "Compiler error:\n%A" errors)

                let errors = AssemblyPostProcessor.eraseCCtors fsiDynamicAssembly.Value outFile

                let assembly =
                    {
                        Assembly = Assembly.ReflectionOnlyLoadFrom outFile
                        PdbFile = pdbFile
                        Interactions = state.InteractionQueue
                        Dependencies = state.Dependencies |> Map.toSeq |> Seq.map snd |> Seq.toList
                    }

                { state with
                    AstQueue = []
                    InteractionQueue = []
                    CompiledAssemblies = state.CompiledAssemblies.Add(an.Name, assembly)
                    // append compiled assembly to dependencies for next compilations
                    Dependencies = state.Dependencies.Add(outFile, assembly.Assembly)

                    LastCompiledAssembly = Some assembly
                    NextCompiledAssembly = getNextAssemblyName (state.CompiledAssemblies.Count + 2)
                }

        // behaviour of the compiler actor
        let rec compilerBehaviour (state : InteractionCompilerState) (inbox : MailboxProcessor<InteractionCompilerMessage>) =      
            async {

                let! message = inbox.Receive()

                match message with
                | GetCompilerState rc ->
                    // return immutable state to the client
                    do rc.Reply state
                    return! compilerBehaviour state inbox

                | NextInteraction(inputs, typedAssembly, references, rc) ->
                    try
                        // a fresh iteration of the repl has started
                        // push ASTs from previous interaction to the queue, if such exists
                        // performing pickling operation on top-level value bindings where possible
                        do tyConv.AddPendingInteractionToModuleIndex state
                        let state = AstTransformer.popPendingInteraction fsiDynamicAssembly.Value false state

                        // make initial analysis of current interaction
                        let pending = TastAnalyzer.extractFsiMetadata fsiDynamicAssembly.Value references typedAssembly inputs
                        let state = { state with PendingInteraction = Some pending }

                        // acknowledge completion
                        do rc.Reply None

                        return! compilerBehaviour state inbox

                    with e ->
                        do rc.Reply <| Some e
                        return! compilerBehaviour state inbox

                | RequestCompilation (compileUnevaluatedInputs, rc) ->
                    try
                        // if requested, append ASTs for current interaction to the compilation queue
                        // do *NOT* pickle values here, as evaluation is still pending
                        let state =
                            if compileUnevaluatedInputs then
                                do tyConv.AddPendingInteractionToModuleIndex state
                                AstTransformer.popPendingInteraction fsiDynamicAssembly.Value false state
                            else
                                state

                        let state = compile state

                        // reply to sender
                        rc.Reply <| Choice1Of2 state.LastCompiledAssembly.Value

                        return! compilerBehaviour state inbox

                    with e ->
                        rc.Reply <| Choice2Of2 e
                        return! compilerBehaviour state inbox
            }

        // initialize the actor
        let compilerActor = 
            let state = InteractionCompilerState.Empty
            let distribFsiCoreAssembly = typeof<FsiCompiledAssembly>.Assembly
            let state = 
                { state with 
                    NextCompiledAssembly = getNextAssemblyName 1
                    Dependencies = state.Dependencies.Add(distribFsiCoreAssembly.Location, distribFsiCoreAssembly)
                }
            MailboxProcessor.Start(compilerBehaviour state)

        let requestCompilation compileUnevaluated =
            match compilerActor.PostAndReply <| fun ch -> RequestCompilation(compileUnevaluated, ch) with
            | Choice1Of2 assembly -> assembly
            | Choice2Of2 e -> raise e

        let getCompilerState () = compilerActor.PostAndReply <| GetCompilerState

        let getTopLevelModuleName(t : Type) = t.FullName.Split([|'.';'+'|]).[0]

        member __.EnqueueInteraction (inputs : ParsedInput list, typedAssembly : TypedAssembly, references : string list) =
            match compilerActor.PostAndReply <| fun rc -> NextInteraction (inputs, typedAssembly, references, rc) with
            | None -> ()
            | Some e -> raise e

        member __.GetCompilerInfo () =
            {
                new InteractionCompilerInfo with
                    member __.ShellId = shellId
                    member __.DynamicAssembly = fsiDynamicAssembly.Value
                    member __.CompiledAssemblyPath = outDirectory

                    member __.Serializer = serializer
                    
                    member __.RequestCompilation compileCurrentInteraction = requestCompilation compileCurrentInteraction

                    member __.TryGetCompiledAssembly(t : Type) = 
                        InteractionAnalysis.tryGetCompiledAssembly fsiDynamicAssembly.Value (getCompilerState()) t

                    member __.TryGetDeclarationInfo(m : MemberInfo) = 
                        InteractionAnalysis.tryBuildDependencyTree fsiDynamicAssembly.Value (getCompilerState()) m
            }