module internal Nessos.Vagrant.AssemblyLoader

    open System
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils


    //
    // assembly loader protocol implementation
    //

    type AssemblyLoader = StatefulActor<Map<AssemblyId, AssemblyLoadInfo>, PortableAssembly, AssemblyLoadInfo>

    /// need an assembly resolution handler when loading assemblies at runtime

    let registerAssemblyResolutionHandler () = 
        System.AppDomain.CurrentDomain.add_AssemblyResolve <|
            new ResolveEventHandler (fun _ args -> defaultArg (tryGetLoadedAssembly args.Name) null)

    /// portable assembly load protocol implementation

    let loadAssembly (pickler : FsPickler) (isLocalDynamicSlice : AssemblyId -> bool)
                        (state : Map<AssemblyId, AssemblyLoadInfo>) (pa : PortableAssembly) =

        // return operators
        let success info = state.Add(pa.Id, info), info
        let error e = state, LoadFault(pa.Id, e)

        // loads the static initializer for given portable assembly
        // requires the assembly to be already loaded in the current AppDomain
        let tryLoadStaticInitializer (previous : StaticInitializationInfo option) (pa : PortableAssembly) =
            let tryLoad (fI : FieldInfo, data : Exn<byte []>) =
                match data with
                | Success bytes ->
                    try
                        let value = pickler.UnPickle<obj> bytes
                        fI.SetValue(null, value) ; None
                    with e -> Some(fI, e)
                | Error e -> Some(fI, e)

            match previous, pa.StaticInitializer with
            | None, None -> Loaded pa.Id
            // keep the previous static initializer if PA has none
            | Some previous, None -> LoadedWithStaticIntialization(pa.Id, previous)

            // silently discard if loaded generation larger than current
            | Some info, Some init when info.Generation > init.Generation -> 
                LoadedWithStaticIntialization(pa.Id, info)

            // perform the static initialization
            | _, Some init ->
                let initializers = pickler.UnPickle<AssemblyExporter.StaticInitializers>(init.Data)
                let errors = Array.choose tryLoad initializers
                let info = { Generation = init.Generation ; Errors = errors ; IsPartial = init.IsPartial }
                LoadedWithStaticIntialization(pa.Id, info)


        let loadAssembly (pa : PortableAssembly) =
            // try load from AppDomain or GAC
            match tryLoadAssembly pa.FullName with
            | Some a when a.AssemblyId = pa.Id -> success <| tryLoadStaticInitializer None pa
            | Some _ -> 
                let msg = sprintf "an incompatible version of '%s' has been loaded in the client." pa.FullName
                error <| VagrantException(msg)
            | None ->
                // try load binary image
                match pa.Image with
                | None -> state, NotLoaded pa.Id
                | Some bytes ->
                    let assembly =
                        match pa.Symbols with
                        | None -> System.Reflection.Assembly.Load(bytes)
                        | Some symbols -> System.Reflection.Assembly.Load(bytes, symbols)

                    if assembly.FullName <> pa.FullName then
                        let msg = sprintf "Expected assembly '%s', received '%s'." pa.FullName assembly.FullName
                        raise <| VagrantException(msg)
                    else
                        
                        success <| tryLoadStaticInitializer None pa

        try
            match state.TryFind pa.Id with
            // dynamic assembly slice generated in local process
            | None when isLocalDynamicSlice pa.Id -> success <| Loaded pa.Id
            // assembly not registered in state, attempt to load now
            | None -> loadAssembly pa
            // assembly loaded with static initializers, attempt to update
            | Some (LoadedWithStaticIntialization (_,info)) ->
                success <| tryLoadStaticInitializer (Some info) pa

            | Some result -> state, result

        with e -> error e
    
    let mkAssemblyLoader pickler tryGetLocal : AssemblyLoader =
        mkStatefulActor Map.empty (fun state pa -> loadAssembly pickler tryGetLocal state pa)


    let getAssemblyLoadInfo (loader : AssemblyLoader) (id : AssemblyId) = loader.PostAndReply <| PortableAssembly.Empty id

    /// assembly receive protocol on the client side
    let assemblyReceiveProtocol (loader : AssemblyLoader) (publisher : IRemoteAssemblyPublisher) = async {
        // step 1. download dependencies required by publisher
        let! dependencies = publisher.GetRequiredAssemblyInfo()

        // step 2. resolve dependencies that are missing from client
        let tryCheckLoadStatus (id : AssemblyId) =
            match getAssemblyLoadInfo loader id with
            | NotLoaded id
            | LoadFault (id,_) -> Some id
            | Loaded _ -> None
            | LoadedWithStaticIntialization _ -> None

        let missing = dependencies |> List.choose tryCheckLoadStatus

        if missing.Length > 0 then
            // step 3. download portable assemblies for missing dependencies
            let! assemblies = publisher.PullAssemblies missing
            let loadResults = assemblies |> List.map loader.PostAndReply

            let checkLoadResult (info : AssemblyLoadInfo) =
                match info with
                | NotLoaded id -> raise <| new VagrantException(sprintf "failed to load assembly '%s'" id.FullName)
                | LoadFault(_, (:? VagrantException as e)) -> raise e
                | LoadFault(id, e) -> raise <| new VagrantException(sprintf "failed to load assembly '%s'" id.FullName, e)
                | Loaded _ | LoadedWithStaticIntialization _ -> ()

            List.iter checkLoadResult loadResults
    }