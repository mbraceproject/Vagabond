module Nessos.Vagrant.SliceCompiler

    open System
    open System.IO
    open System.Text.RegularExpressions
    open System.Reflection

    open Mono.Cecil
    open Mono.Collections.Generic
    open Mono.Reflection

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.TypeRefUpdater
    open Nessos.Vagrant.DependencyAnalysis

    let tryUpdateTypeReference (assembly : AssemblyDefinition) (state : GlobalDynamicAssemblyState) (t : TypeReference) =
        if t = null then None
        else
            let assemblyName = defaultArg t.ContainingAssembly assembly.FullName

            match state.DynamicAssemblies.TryFind assemblyName with
            | None -> None
            | Some info ->
                let name = t.CanonicalName
                match info.TypeIndex.TryFind name with
                | None -> None
                | Some a ->
                    let rt = a.Assembly.GetType(name, true)
                    let tI = assembly.MainModule.Import(rt)
                    Some tI


    let initGlobalState (outDirectory : string) =
        let uuid = Guid.NewGuid()
        let assemblyRegex = Regex(sprintf "^(.*)_%O_[0-9]*" uuid)
        let tryExtractDynamicAssemblyName (assemblyName : string) =
            let m = assemblyRegex.Match(assemblyName)
            if m.Success then Some <| m.Groups.[1].Value
            else
                None
        {
            ServerId = Guid.NewGuid()
            OutputDirectory = outDirectory
            TryGetDynamicAssemblyName = tryExtractDynamicAssemblyName
            DynamicAssemblies = Map.empty
        }

    //
    //  TODO: for performance reasons, it would be a good idea to merge Mono.Reflection's AssemblySaver and the slicer
    //  The *entire* dynamic assembly will be parsed every time a slice is requested.
    //
    //  traverse the type hierarchy to: 
    //  1) identify and collect all freshly emitted type definitions
    //  2) for all the previously emitted types:
    //      a) erase type definitions that do not have new nested types
    //      b) keep all others for scaffolding
    //

    let mkAssemblyDefinitionSlice (state : DynamicAssemblyState) (assemblyDef : AssemblyDefinition) =

        let eraseTypeContents(t : TypeDefinition) =
            t.Methods.Clear() ; t.Fields.Clear()
            t.Events.Clear() ; t.Properties.Clear()
            t.CustomAttributes.Clear()
            for gt in t.GenericParameters do
                gt.CustomAttributes.Clear()
                gt.Constraints.Clear()

        let rec gatherFreshTypes (types : Collection<TypeDefinition>) =
            seq {
                let erasedTypes = ref []

                for t in types do
                    let name = t.CanonicalName
                    
                    if name = "<Module>" then ()
                    elif state.TypeIndex.ContainsKey name then
                        // type has already been emitted, is to be kept only in event of new nested subtypes
                        let freshNested = gatherFreshTypes t.NestedTypes |> Seq.toArray

                        if freshNested.Length = 0 then
                            // no new nested subtypes, schedule for erasure
                            erasedTypes := t :: !erasedTypes
                        else
                            // has nested subtypes, keep type for scaffolding but erase contents
                            yield! freshNested
                            do eraseTypeContents t
                    else
                        // new type, collect the entire hierarchy
                        yield! collectHierarchy t

                // Mono.Collection cannot be updated while enumerated, perform erasure now
                for t in !erasedTypes do 
                    types.Remove(t) |> ignore
            }

        and collectHierarchy (t : TypeDefinition) =
            seq {
                yield t
                for nt in t.NestedTypes do
                    yield! collectHierarchy nt
            }

        gatherFreshTypes assemblyDef.MainModule.Types |> Seq.toArray



    let allStatic = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static


    let eraseStaticInitializers (eraseF : Type -> bool) (assembly : Assembly) (typeDef : TypeDefinition) =
        // static initializers for generic types not supported
        if typeDef.GenericParameters.Count > 0 then [||]
        else
            let name = typeDef.CanonicalName
            let reflectionType = assembly.GetType(name, true)
            if eraseF reflectionType then
                match typeDef.Methods |> Seq.tryFind(fun m -> m.Name = ".cctor") with
                | Some cctor -> typeDef.Methods.Remove(cctor) |> ignore
                | None -> ()

                reflectionType.GetFields(allStatic)
                |> Array.filter (fun f -> not f.IsLiteral)

            else [||]

    let mkSliceInfo (dynamicAssembly : Assembly) (sliceId : int) (staticFields : FieldInfo [] []) (slice : Assembly) =
        let mapFieldInfo (dynamicFields : FieldInfo []) =
            if dynamicFields.Length = 0 then [||]
            else
                let declaringType = dynamicFields.[0].DeclaringType
                let mappedDeclaringType = slice.GetType(declaringType.FullName, true)
                dynamicFields 
                |> Array.map (fun fI -> 
                    match mappedDeclaringType.GetField(fI.Name, allStatic) with 
                    | null -> failwithf "field '%O' was null." fI 
                    | fI' -> fI,fI')

        let staticFields = staticFields |> Seq.collect mapFieldInfo |> Seq.toList

        {
            Assembly = slice
            DynamicAssemblyName = dynamicAssembly.FullName
            StaticFields = staticFields
            SliceId = sliceId
        }

    let tryCompileDynamicAssemblySlice (state : GlobalDynamicAssemblyState) (assembly : Assembly) =
        
        if not assembly.IsDynamic then invalidArg assembly.FullName "not a dynamic assembly."

        let assemblyState =
            match state.DynamicAssemblies.TryFind assembly.FullName with
            | None -> DynamicAssemblyState.Init assembly
            | Some info -> info

        if not assemblyState.HasFreshTypes then None
        else
            // parse dynamic assembly
            let snapshot = AssemblySaver.Read assemblyState.DynamicAssembly

            // update snapshot to only contain current slice
            let freshTypes = mkAssemblyDefinitionSlice assemblyState snapshot

            // remap typeRefs so that slices are correctly referenced
            do remapTypeReferences (memoize <| tryUpdateTypeReference snapshot state) freshTypes

            // erase type initializers where applicable
            let staticFields = freshTypes |> Array.map (eraseStaticInitializers (fun _ -> true) assemblyState.DynamicAssembly)

            // compile

            let sliceId = assemblyState.GeneratedSlices.Length + 1
            let name = sprintf "%s_%O_%03d" assemblyState.Name.Name state.ServerId sliceId
            let target = System.IO.Path.Combine(state.OutputDirectory, name + ".dll")

            do snapshot.Name.Name <- name
            do snapshot.Write(target)

            // update type index & compiled assembly info
            let assembly = Assembly.ReflectionOnlyLoadFrom(target)
            let sliceInfo = mkSliceInfo assemblyState.DynamicAssembly sliceId staticFields assembly
            let generatedSlices = sliceInfo :: assemblyState.GeneratedSlices
            let typeIndex = freshTypes |> Seq.map (fun t -> t.CanonicalName, sliceInfo) |> Map.addMany assemblyState.TypeIndex
            

            let assemblyState = { assemblyState with GeneratedSlices = generatedSlices ; TypeIndex = typeIndex }
            let dynamicAssemblyIndex = state.DynamicAssemblies.Add(assemblyState.DynamicAssembly.FullName, assemblyState)
            let state = { state with DynamicAssemblies = dynamicAssemblyIndex}

            Some (sliceInfo, state)


//    let compileDynamicAssemblySlices (state : GlobalDynamicAssemblyState) (assemblies : seq<Assembly>) =
//        let foldF ((state : GlobalDynamicAssemblyState, slices : AssemblySliceInfo list) as s) (a : Assembly) =
//            match tryCompileDynamicAssemblySlice state a with
//            | None -> s
//            | Some(slice',state') -> (state', slice' :: slices)
//            
//        // need to find all dynamic assemblies in dependency tree
//        let dynamicAssemblies = assemblies |> traverseDependencies |> List.filter (fun a -> a.IsDynamic)
//
//        let state, slices = List.fold foldF (state, []) dynamicAssemblies
//
//        List.rev slices, state


    type ServerMsg = Assembly * AsyncReplyChannel<Choice<AssemblySliceInfo option, exn>>

    type SliceCompilationServer (?outPath : string) =

        let outPath = 
            match outPath with
            | Some path when Directory.Exists path -> path
            | Some _ -> invalidArg "outPath" "not a valid directory."
            | None -> Path.GetTempPath()

        let globalStateContainer = ref <| initGlobalState outPath

        // sequentialize compilation requests
        let rec compiler (mailbox : MailboxProcessor<ServerMsg>) = async {
            let! assembly, rc = mailbox.Receive()

            let result =
                try
                    match tryCompileDynamicAssemblySlice globalStateContainer.Value assembly with
                    | None -> Choice1Of2 None
                    | Some (slice,state) ->
                        do globalStateContainer := state
                        Choice1Of2 <| Some slice

                with e -> Choice2Of2 e

            rc.Reply result

            return! compiler mailbox
        }

        let compilationActor = MailboxProcessor.Start compiler

        let tryCompileNextSlice (a : Assembly) =
            match compilationActor.PostAndReply <| fun ch -> a,ch with
            | Choice1Of2 slice -> slice
            | Choice2Of2 e -> raise e

        member __.UUId = globalStateContainer.Value.ServerId
        member __.State = !globalStateContainer

        member __.CompileNextSlices (assemblies : seq<Assembly>) =
            assemblies 
            |> traverseDependencies 
            |> List.filter(fun a -> a.IsDynamic)
            |> List.choose tryCompileNextSlice