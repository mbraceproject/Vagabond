module internal Nessos.Vagrant.AssemblyExporter

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils

    let exportAssembly (pickler : FsPickler) (state : DynamicAssemblyCompilerState) (generationIdx : Map<string, int>)
                        includeImage includeStaticInitializationData (assembly : Assembly) =

        let mkPortableAssembly dynamicAssemblyInfo =
            {
                FullName = assembly.FullName
                Image = if includeImage then Some <| File.ReadAllBytes assembly.Location else None
                DynamicAssemblyInfo = dynamicAssemblyInfo
            }
                 
        match state.TryFindSliceInfo assembly.FullName with
        | Some (dynAssembly, sliceInfo) ->

            let generation = 1 + defaultArg (generationIdx.TryFind assembly.FullName) -1
            let isPartiallyEvaluated = 
                dynAssembly.Profile.IsPartiallyEvaluatedSlice (fun t -> dynAssembly.TypeIndex.TryFind t.FullName) sliceInfo

            let generationIdx, staticInitializer =
                if includeStaticInitializationData && sliceInfo.StaticFields.Length > 0 then
                    // create pickled type initializers
                    let tryPickle (fI : FieldInfo) =
                        try
                            let value = fI.GetValue(null)
                            let pickle = pickler.Pickle<obj>(value)
                            Choice1Of2 (fI, pickle)
                        with e -> 
                            Choice2Of2 (fI, e)

                    let pickles, errors = sliceInfo.StaticFields |> Array.map tryPickle |> Seq.toList |> Choice.partition2
                    let generationIdx = generationIdx.Add(assembly.FullName, generation)
                    let staticInitializer = { Pickles = Array.ofList pickles ; Errors = Array.ofList errors }
                    generationIdx, Some staticInitializer
                else
                    generationIdx, None

            let dynamicAssemblyInfo =
                {
                    SourceId = state.ServerId
                    DynamicAssemblyName = sliceInfo.DynamicAssemblyQualifiedName
                    SliceId = sliceInfo.SliceId

                    IsPartiallyEvaluated = isPartiallyEvaluated
                    Generation = generation
                    StaticInitializationData = staticInitializer
                }

            generationIdx, mkPortableAssembly (Some dynamicAssemblyInfo)

        | _ -> generationIdx, mkPortableAssembly None


    type LoadedAssemblyInfo = Assembly * DynamicAssemblyInfo option

    let loadAssembly (pickler : FsPickler) (localId : Guid option) (state : Map<string, LoadedAssemblyInfo>) (pa : PortableAssembly) =

        let tryLoadAssembly (state : Map<string, LoadedAssemblyInfo>) (pa : PortableAssembly) =
            let tryLoadFromAppDomain (pa : PortableAssembly) =
                System.AppDomain.CurrentDomain.GetAssemblies()
                |> Array.tryFind (fun a -> a.FullName = pa.FullName)

            match state.TryFind pa.FullName with
            | Some _ as result -> state, result
            | None ->
                let assembly =
                    match tryLoadFromAppDomain pa, pa.Image with
                    | Some _ as a, _ -> a
                    | None, Some img -> Some <| System.Reflection.Assembly.Load img
                    | None, None -> None

                match assembly with
                | Some a ->
                    let contents = a, pa.DynamicAssemblyInfo
                    let state = state.Add(a.FullName, info)
                    state, Some info
                | None -> state, None

        let tryLoadStaticInitializers (info : LoadedAssemblyInfo) (data : StaticInitializationData) =
            match info.Generation with
            // silently discard if static initializers are stale
            | Some gen when gen >= data.Generation -> [||], info
            | _ ->
                let tryLoad (fI : FieldInfo, bytes : byte []) =
                    try 
                        let value = pickler.UnPickle<obj> bytes
                        fI.SetValue(null, value) ; None
                    with e -> Some (fI, e)

                let errors = data.Pickles |> Array.choose tryLoad
                let info2 = { info with Generation = Some data.Generation ; IsPartiallyEvaluated = data.IsPartiallyEvaluated }

                errors, info2


        match localId, pa.DynamicAssemblySourceId with
        // bypass if dynamic assemblies emitted locally
        | Some id, Some id' when id = id' -> state, Loaded
        | _ ->
            let state, assemblyInfo = tryLoadAssembly state pa
            match assemblyInfo with
            | None -> state, MissingImage
            | Some info ->
                // load static initializers
                match pa.StaticInitializationData with
                | None when info.IsPartiallyEvaluated -> state, MissingStaticInitializer
                | None -> state, Loaded
                    
                    
                    
//            match loadedAssemblies.TryFind pa.FullName, pa. with
//            | Some info
//            match loadedAssemblies.TryFind pa.FullName with
//            | None when not <| isLoadedInAppDomain pa ->
//                // assembly image is not loaded, try to load now
//                match pa.Image with
//                | None -> loadedAssemblies, MissingImage
//                | Some img -> 
//                    let a = System.Reflection.Assembly.Load(img)
//                    assert (a.FullName = pa.FullName)

        
        

//    let exportDynamicAssemblySlice includeImage includeStaticInitializationData (slice : DynamicAssemblySlice) =
//        let package = exportStaticAssembly includeImage slice.Assembly
//        
//        if includeStaticInitializationData then
//            