module internal Nessos.Vagrant.AssemblyLoader

    open System
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.AssemblyExporter
    open Nessos.Vagrant.AssemblyStore


    //
    // assembly loader protocol implementation
    //

    type AssemblyLoadState = Map<AssemblyId, AssemblyLoadInfo>

//    type AssemblyLoader = StatefulActor<Map<AssemblyId, AssemblyLoadInfo>, PortableAssembly * bool * AssemblyLocalResolutionPolicy, AssemblyLoadInfo>

    /// need an assembly resolution handler when loading assemblies at runtime

//    let registerAssemblyResolutionHandler () = 
//        System.AppDomain.CurrentDomain.add_AssemblyResolve <|
//            new ResolveEventHandler (fun _ args -> defaultArg (tryGetLoadedAssembly args.Name) null)

    /// portable assembly load protocol implementation

//    let loadAssembly (pickler : BasePickler) (isLocalDynamicSlice : AssemblyId -> bool)
//                        (state : Map<AssemblyId, AssemblyLoadInfo>) 
//                        (loadPolicy : AssemblyLocalResolutionPolicy) (requireIdenticalAssembly : bool)
//                        (pa : PortableAssembly) =
    let loadAssembly (pickler : BasePickler) (store : AssemblyStore)
                        (isLocalDynamicAssemblySlice : AssemblyId -> bool)
                        (loadState : AssemblyLoadState) (policy : AssemblyLoadPolicy)
                        (pa : PortableAssembly) =

        // return operators
        let success info = loadState.Add(pa.Id, info), info
        let error e = loadState, LoadFault(pa.Id, e)

        // parse load policies
        let loadInAppDomain = not <| policy.HasFlag AssemblyLoadPolicy.CacheOnly
        let requireIdentical = policy.HasFlag AssemblyLoadPolicy.RequireIdentical
        let tryResolveLocally =
            if policy.HasFlag AssemblyLoadPolicy.ResolveAll then true
            elif policy.HasFlag AssemblyLoadPolicy.ResolveStrongNames then
                pa.Id.IsStrongAssembly
            else
                false

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
            | None, None -> Loaded (pa.Id, true, None)
            // keep the previous static initializer if PA has none
            | Some previous, None -> Loaded(pa.Id, true, Some previous)
            // silently discard if loaded generation larger than current
            | Some info, Some init when info.Generation > init.Generation ->  Loaded(pa.Id, true, Some info)

            // perform the static initialization
            | _, Some init ->
                let initializers = pickler.UnPickle<AssemblyExporter.StaticInitializers>(init.Data)
                let errors = Array.choose tryLoad initializers
                let info = { Generation = init.Generation ; Errors = errors ; IsPartial = init.IsPartial }
                Loaded(pa.Id, true, Some info)


        let loadAssembly (pa : PortableAssembly) =

            // Attempt resolving locally
            let localAssembly =
                if tryResolveLocally then
                    tryLoadAssembly pa.FullName
                else
                    tryGetLoadedAssembly pa.FullName

            match localAssembly with
            // if specified, check if loaded assembly has identical image hash
            | Some a when requireIdentical && a.AssemblyId <> pa.Id ->
                let msg = sprintf "an incompatible version of '%s' has been loaded." pa.FullName
                raise <| VagrantException(msg)

            // if GAC, do not cache, just report as loaded
            | Some a when a.GlobalAssemblyCache -> success <| Loaded (pa.Id, true, None)
            
            // local assemblies not in GAC are to be cached
            | Some a ->
                let info =
                    if pa.Image.IsSome then
                        cachePortableAssembly pickler cacheEntry pa
                    else
                        let pa' = mkPortableAssembly true None a
                        cachePortableAssembly pickler cacheEntry { pa' with Id = pa.Id }
                    
                success (AssemblyLoadInfo.SetIsLoaded info)

            | None when pa.Image.IsSome ->
                // cache assembly and load from location
                match cachePortableAssembly pickler cacheEntry pa with
                | Loaded(_,_,info) when loadInAppDomain ->
                    let assembly = System.Reflection.Assembly.LoadFrom cacheEntry.Assembly

                    if assembly.FullName <> pa.FullName then
                        let msg = sprintf "Expected assembly '%s', received '%s'." pa.FullName assembly.FullName
                        raise <| VagrantException(msg)

                    elif requireIdentical && assembly.AssemblyId <> pa.Id then
                        let msg = sprintf "an incompatible version of '%s' has been loaded." pa.FullName
                        raise <| VagrantException(msg)

                    else
                        success <| tryLoadStaticInitializer None pa

                | info -> success info

            | None -> success <| NotLoaded pa.Id

        try
            match loadState.TryFind pa.Id with
            // dynamic assembly slice generated in local process
            | None when isLocalDynamicAssemblySlice pa.Id -> success <| Loaded (pa.Id, true, None)
            // assembly not registered in state, attempt to load now
            | None -> loadAssembly pa
            // assembly loaded with static initializers, attempt to update
            | Some (Loaded(id, isLoadedLocal, Some info) as l) ->
                if not isLoadedLocal && loadInAppDomain then
                    loadAssembly pa

                else
                    match pa.StaticInitializer with
                    | None -> loadState, l
                    | Some init ->
                        let cacheInfo = writeStaticInitializer pickler cacheEntry (Some info) init
                        
                        if loadInAppDomain || isLoadedLocal then
                            success <| tryLoadStaticInitializer (Some info) pa

                        else
                            success <| Loaded(id, false, Some cacheInfo)

            | Some result -> loadState, result

        with e -> error e
    
//    let mkAssemblyLoader pickler tryGetLocal : AssemblyLoader =
//        mkStatefulActor Map.empty (fun state (pa, policy, req) -> loadAssembly pickler tryGetLocal state req policy pa)


//    let getAssemblyLoadInfo (loader : AssemblyLoader) requireIdentical loadPolicy (id : AssemblyId) = 
//        loader.PostAndReply (PortableAssembly.Empty id, requireIdentical, loadPolicy)
//
//    /// assembly receive protocol on the client side
//    let assemblyReceiveProtocol (loader : AssemblyLoader) requireIdentical loadPolicy (publisher : IRemoteAssemblyPublisher) = async {
//        // step 1. download dependencies required by publisher
//        let! dependencies = publisher.GetRequiredAssemblyInfo()
//
//        // step 2. resolve dependencies that are missing from client
//        let tryCheckLoadStatus (id : AssemblyId) =
//            match getAssemblyLoadInfo loader requireIdentical loadPolicy id with
//            | NotLoaded id
//            | LoadFault (id,_) -> Some id
//            | Loaded _ -> None
//            | LoadedWithStaticIntialization _ -> None
//
//        let missing = dependencies |> List.choose tryCheckLoadStatus
//
//        if missing.Length > 0 then
//            // step 3. download portable assemblies for missing dependencies
//            let! assemblies = publisher.PullAssemblies missing
//            let loadResults = assemblies |> List.map (fun a -> loader.PostAndReply(a, requireIdentical, loadPolicy))
//
//            let checkLoadResult (info : AssemblyLoadInfo) =
//                match info with
//                | NotLoaded id -> raise <| new VagrantException(sprintf "failed to load assembly '%s'" id.FullName)
//                | LoadFault(_, (:? VagrantException as e)) -> raise e
//                | LoadFault(id, e) -> raise <| new VagrantException(sprintf "failed to load assembly '%s'" id.FullName, e)
//                | Loaded _ | LoadedWithStaticIntialization _ -> ()
//
//            List.iter checkLoadResult loadResults
//    }