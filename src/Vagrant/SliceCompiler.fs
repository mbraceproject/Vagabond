module internal Nessos.Vagrant.SliceCompiler

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



    let initCompilerState (outDirectory : string) =
        let uuid = Guid.NewGuid()
        let mkSliceName (name : string) (id : int) = sprintf "%s_%O_%d" name uuid id
        let assemblyRegex = Regex(sprintf "^(.*)_%O_[0-9]+" uuid)
        let tryExtractDynamicAssemblyName (assemblyName : string) =
            let m = assemblyRegex.Match(assemblyName)
            if m.Success then Some <| m.Groups.[1].Value
            else
                None
        {
            ServerId = Guid.NewGuid()
            OutputDirectory = outDirectory
            DynamicAssemblies = Map.empty

            TryGetDynamicAssemblyName = tryExtractDynamicAssemblyName
            CreateAssemblySliceName = mkSliceName
        }

    let tryUpdateTypeReference (assembly : AssemblyDefinition) (state : GlobalDynamicAssemblyState) (t : TypeReference) =
        if t = null then None
        else
            let assemblyName = defaultArg t.ContainingAssembly assembly.FullName

            match state.DynamicAssemblies.TryFind assemblyName with
            | None -> None // referenced assembly not dynamic
            | Some info ->
                let name = t.CanonicalName
                match info.TypeIndex.TryFind name with
                | None when assemblyName = assembly.FullName -> None // the type will be compiled in current slice; do not remap
                | None -> failwithf "Vagrant error: referencing type '%O' from dynamic assembly '%s' which has not been sliced." name assemblyName
                | Some a ->
                    let rt = a.Assembly.GetType(name, true)
                    let tI = assembly.MainModule.Import(rt)
                    Some tI

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

    let compileDynamicAssemblySlice (state : GlobalDynamicAssemblyState) 
                                        (dynamicAssembly : Assembly) 
                                        (dependencies : Assembly list) 
                                        (snapshot : AssemblyDefinition) =

        let assemblyState =
            match state.DynamicAssemblies.TryFind dynamicAssembly.FullName with
            | None -> DynamicAssemblyState.Init(dynamicAssembly)
            | Some info -> info

        // update snapshot to only contain current slice
        let freshTypes = mkAssemblyDefinitionSlice assemblyState snapshot

        // remap typeRefs so that slices are correctly referenced
        do remapTypeReferences (memoize <| tryUpdateTypeReference snapshot state) freshTypes

        // erase type initializers where applicable
        let staticFields = freshTypes |> Array.map (eraseStaticInitializers (fun _ -> true) assemblyState.DynamicAssembly)

        // compile

        let sliceId = assemblyState.GeneratedSlices.Count + 1
        let name = state.CreateAssemblySliceName assemblyState.Name.Name sliceId
        let target = System.IO.Path.Combine(state.OutputDirectory, name + ".dll")

        do snapshot.Name.Name <- name
        do snapshot.Write(target)

        // update type index & compiled assembly info
        let assembly = Assembly.ReflectionOnlyLoadFrom(target)
        let sliceInfo = mkSliceInfo assemblyState.DynamicAssembly sliceId staticFields assembly
        let generatedSlices = assemblyState.GeneratedSlices.Add(assembly.FullName, sliceInfo)
        let typeIndex = freshTypes |> Seq.map (fun t -> t.CanonicalName, sliceInfo) |> Map.addMany assemblyState.TypeIndex

        let assemblyState = { assemblyState with GeneratedSlices = generatedSlices ; TypeIndex = typeIndex ; AssemblyReferences = dependencies }
        let dynamicAssemblyIndex = state.DynamicAssemblies.Add(assemblyState.DynamicAssembly.FullName, assemblyState)
        let state = { state with DynamicAssemblies = dynamicAssemblyIndex}

        sliceInfo, state


    let compileDynamicAssemblySlices (state : GlobalDynamicAssemblyState) (assemblies : Assembly list) =
        // resolve dynamic assembly dependency graph
        let parsedDynamicAssemblies = parseDynamicAssemblies state assemblies

        // the returned state should reflect the last successful compilation
        let compileSlice (state : GlobalDynamicAssemblyState, acc : Choice<AssemblySliceInfo list, exn>) 
                            (dynamic : Assembly, snapshot : AssemblyDefinition, references : Assembly list) =
            match acc with
            | Choice1Of2 slices ->
                try
                    let slice, state = compileDynamicAssemblySlice state dynamic references snapshot
                    state, Choice1Of2 (slice :: slices)
                with e ->
                    state, Choice2Of2 e
            | Choice2Of2 _ -> state, acc // discard the remaining list

        List.fold compileSlice (state, Choice1Of2 []) parsedDynamicAssemblies