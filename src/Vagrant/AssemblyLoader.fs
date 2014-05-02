module internal Nessos.Vagrant.AssemblyLoader

    open System
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils


    //
    // assembly loader protocol implementation
    //

//    type LoadedAssemblyInfo =
//        | UnLoaded
//        | LoadedStatic of AssemblyId
//        | LoadedDynamic of requiresStaticInitialization:bool * generation:int option * isPartiallyEvaluated:bool


    /// need an assembly resolution handler when loading assemblies at runtime

    let registerAssemblyResolutionHandler () = 
        System.AppDomain.CurrentDomain.add_AssemblyResolve <| 
            new ResolveEventHandler (fun _ args -> defaultArg (tryGetLoadedAssembly args.Name) null)

    /// portable assembly load protocol implementation

    let rec loadAssembly (pickler : FsPickler) (localId : Guid option) (state : Map<string, AssemblyInfo>) (pa : PortableAssembly) =

        let loadStaticInitializer (data : byte []) =
            let tryLoad (fI : FieldInfo, data : Exn<byte []>) =
                match data with
                | Success bytes ->
                    try
                        let value = pickler.UnPickle<obj> bytes
                        fI.SetValue(null, value) ; None
                    with e -> Some(fI, e)
                | Error e -> Some(fI, e)

            let initializers = pickler.UnPickle<AssemblyExporter.StaticInitializers>(data)
            Array.choose tryLoad initializers

        // load assembly image
        let loadAssembly (pa : PortableAssembly) =
            match tryLoadAssembly pa.FullName with
            | Some a when a.AssemblyId = pa.Id -> ()
            | Some _ -> raise <| new VagrantException(sprintf "an incompatible version of '%s' has been loaded in the client." pa.FullName)
            | None ->
                // try load binary image
                match pa.Image with
                | None -> raise <| new VagrantException(sprintf "portable assembly '%s' missing binary image." pa.FullName)
                | Some bytes ->
                    let assembly =
                        try
                            match pa.Symbols with
                            | None -> System.Reflection.Assembly.Load(bytes)
                            | Some symbols -> System.Reflection.Assembly.Load(bytes, symbols)

                        with e -> raise <| new VagrantException(sprintf "Error loading assembly '%s.'" pa.FullName, e)

                    if assembly.FullName <> pa.FullName then
                        let msg = sprintf "Expected assembly '%s', received '%s'." pa.FullName assembly.FullName
                        raise <| new VagrantException(msg)

            let staticInitializerErrors =
                match pa.StaticInitializer with
                | None -> [||]
                | Some (_,data) -> loadStaticInitializer data

            LoadSuccess(pa.Id, staticInitializerErrors)


        let updateWith info = state.Add(pa.FullName, info)
            
        match localId, state.TryFind pa.FullName, pa.Info.DynamicAssemblySliceInfo with
        // dynamic assembly slice generated in local process
        | Some id, _, Some dyn when dyn.SourceId = id -> state, LoadSuccess (pa.Id, [||])
        // assembly not registered in state
        | _, None, _ ->
            let result = loadAssembly pa
            updateWith pa.Info, result
        // assembly registered in state
        | _, Some currentInfo, _ ->
            match currentInfo.DynamicAssemblySliceInfo, pa.Info.DynamicAssemblySliceInfo with
            | None, None -> state, LoadSuccess (pa.Id, [||])
            | Some previous, Some current ->
                match pa.StaticInitializer with
                | None when previous.RequiresStaticInitialization -> 
                    raise <| new VagrantException(sprintf "Portable assembly '%s' missing static initialization data." pa.FullName)
                | None -> state, LoadSuccess (pa.Id, [||])
                | Some(gen,data) when gen > previous.StaticInitializerGeneration ->
                    let staticInitializerErrors = loadStaticInitializer data
                    updateWith pa.Info, LoadSuccess(pa.Id, staticInitializerErrors)

                | Some _ -> state, LoadSuccess(pa.Id, [||])

            | _ -> raise <| new VagrantException(sprintf "Invalid data for portable assembly '%s'." pa.FullName)
            
           
//        let updateState (pa : PortableAssembly) status = state.Add(pa.FullName, status)
//        let currentStatus = defaultArg (state.TryFind pa.FullName) UnLoaded
//
//        try
//            match localId, currentStatus, pa.DynamicAssemblyInfo with
//            // static assembly already loaded in state
//            | _, LoadedStatic id', _ when pa.Id = id' -> state, Loaded (pa.Id, [||])
//            | _, LoadedStatic _, _ -> raise <| new VagrantException(sprintf "an incompatible version of '%s' has been loaded in the client." pa.FullName)
//
//            // static assembly not registered in state
//            | _, UnLoaded, None ->
//                match tryLoadAssembly pa with
//                | None -> state, MissingAssemblyImage pa.Id
//                | Some a ->
//                    updateState pa (LoadedStatic pa.Id), Loaded (pa.Id, [||])
//
//            // trying to load local dynamic assembly
//            | Some localId, _, Some dynInfo when dynInfo.SourceId = localId -> state, Loaded(pa.Id, [||])
//            // dynamic assembly slice, not already loaded in state
//            | _, UnLoaded, Some info ->
//                match tryLoadAssembly pa with
//                | None -> state, MissingAssemblyImage pa.Id
//                | Some a ->
//                    // assembly loaded in AppDomain, update state and recurse
//                    let state = updateState pa <| LoadedDynamic(info.RequiresStaticInitialization, None, info.IsPartiallyEvaluated)
//                    loadAssembly pickler localId state pa
//
//            // loaded dynamic assemblies that do not require static initializers
//            | _, LoadedDynamic(requiresStaticInitialization = false), _ -> state, Loaded(pa.Id, [||])
//
//            // loaded dynamic assemblies without dynamic assembly info
//            | _, LoadedDynamic(requiresStaticInitialization = true ; generation = gen ; isPartiallyEvaluated = isPartial), None ->
//                match gen with
//                | None -> state, MissingStaticInitializer(pa.Id, None)
//                | Some g ->
//                    if isPartial || g < pa.Id.Generation then
//                        state, MissingStaticInitializer(pa.Id, None)
//                    else
//                        state, Loaded(pa.Id, [||])
//
//            // loaded dynamic assembly slices without any previous initialization data
//            | _, LoadedDynamic(requiresStaticInitialization = true ; generation = None), Some info ->
//                match info.StaticInitializerData with
//                | None -> state, MissingStaticInitializer (pa.Id, None)
//                | Some (gen, data) ->
//                    let errors = loadStaticInitializer data
//                    let state = updateState pa <| LoadedDynamic(true, Some gen, info.IsPartiallyEvaluated)
//                    state, Loaded(pa.Id, errors)
//
//            // load type initializers when previous initialization is present
//            | _, LoadedDynamic(requiresStaticInitialization = true ; generation = Some previousGen ; isPartiallyEvaluated = isPartial), Some current ->
//                // silently discard updates if generation is older than currently loaded
//                if previousGen > pa.Id.Generation then state, Loaded (pa.Id, [||])
//                else
//                    match current.StaticInitializerData with
//                    // demand static initializer update if previous push was declared as partial
//                    | None when isPartial -> state, MissingStaticInitializer (pa.Id, Some previousGen)
//                    | None -> state, Loaded (pa.Id, [||])
//                    | Some (gen, data) ->
//                        let errors = loadStaticInitializer data
//                        let state = updateState pa <| LoadedDynamic(true, Some gen, current.IsPartiallyEvaluated)
//                        state, Loaded (pa.Id, errors)
//
//        with e ->
//            state, LoadFault(pa.Id, e)




    type AssemblyLoader = StatefulActor<Map<string, AssemblyInfo>, PortableAssembly, AssemblyLoadResponse>
    
    let mkAssemblyLoader pickler (serverId : Guid option) : AssemblyLoader =
        mkStatefulActor Map.empty (fun state pa -> loadAssembly pickler serverId state pa)
