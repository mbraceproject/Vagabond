module internal Nessos.Vagrant.AssemblyCache

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils

//    type AssemblyCache = StatefulActor<Map<AssemblyId, bool * AssemblyLoadInfo>, PortableAssembly * bool * AssemblyLocalResolutionPolicy, AssemblyLoadInfo>

    type CachePath =
        {
            Assembly : string
            Pdb : string
            StaticInitializer : string
            Metadata : string
        }
    with
        /// resolve unique paths for provided assembly in given cache directory
        static member Resolve (cacheDir : string) (id : AssemblyId) =
            let hash = Convert.toBase32String id.ImageHash
            let name = sprintf "%s-%s" (id.GetName().Name) hash
            let basePath = Path.Combine(cacheDir, name)
            {
                Assembly = basePath + ".dll"
                Pdb = basePath + ".pdb"
                StaticInitializer = basePath + ".init"
                Metadata = basePath + ".vagrant"
            }

    /// write dynamic assembly metadata to cache
    let writeMetadata (pickler : BasePickler) (path : CachePath) info =
        use fs = new FileStream(path.Metadata, FileMode.Create)
        pickler.Serialize<StaticInitializationInfo>(fs, info)

    /// read dynamic assembly metadata from cache
    let tryReadMetadata (pickler : BasePickler) (path : CachePath) =
        if File.Exists path.Metadata then
            use fs = new FileStream(path.Metadata, FileMode.Open)
            let metadata = pickler.Deserialize<StaticInitializationInfo>(fs)
            Some metadata
        else
            None


    let getAssemblyCacheState (pickler : BasePickler) (path : CachePath) (id : AssemblyId) =
        try
            if not <| File.Exists path.Assembly then NotLoaded id
            else
                match tryReadMetadata pickler path with
                | Some info when File.Exists path.StaticInitializer -> LoadedWithStaticIntialization(id, info)
                | Some _ ->
                    let msg = sprintf "cache error: missing static initialization file for assembly '%s'" id.FullName
                    LoadFault(id, VagrantException(msg))

                | None -> Loaded id

        with e -> LoadFault(id, e)

    /// write new static initializer to cache
    let writeStaticInitializer (pickler : BasePickler) (path : CachePath) (previous : StaticInitializationInfo option) (init : StaticInitializer) =
        match previous with
        | Some p when p.Generation > init.Generation -> p
        | _ ->
            let info = { Generation = init.Generation ; Errors = [||] ; IsPartial = init.IsPartial }
            writeMetadata pickler path info
            File.WriteAllBytes(path.StaticInitializer, init.Data)
            info

    /// write portable assembly to cache
    let cachePortableAssembly (pickler : BasePickler) (path : CachePath) (pa : PortableAssembly) =
        match getAssemblyCacheState pickler path pa.Id with
        | Loaded _ -> ()
        | LoadedWithStaticIntialization(id, info) ->
            match pa.StaticInitializer with
            | Some init -> writeStaticInitializer pickler path (Some info) init |> ignore
            | None -> ()

        | _ ->

        match pa.Image with
        | None -> invalidOp <| sprintf "Portable assembly '%O' lacking image." pa.Id
        | Some img ->
            do File.WriteAllBytes(path.Assembly, img)

            match pa.Symbols with
            | None -> ()
            | Some symbols -> File.WriteAllBytes(path.Pdb, symbols)

            // cache the static initializer
            match pa.StaticInitializer with
            | None -> ()
            | Some init -> writeStaticInitializer pickler path None init |> ignore

//    /// the main portable assembly method
//    let cachePortableAssembly (pickler : BasePickler) (cacheDir : string) 
//                                (state : Map<AssemblyId, bool * AssemblyLoadInfo>) 
//                                requireIdentical loadPolicy (pa : PortableAssembly) =
//
//        try
//            let path = CachePath.Resolve cacheDir pa.Id
//
//            let isLoadedInAppDomain, isFirstAccess, loadState =
//                match state.TryFind pa.Id with
//                | None -> 
//                    let isAppDomain, loadState = resolveCachedAssemblyInfo pickler path requireIdentical loadPolicy pa.Id
//                    isAppDomain, true, loadState
//                | Some (isAppDomain, loadState) -> isAppDomain, false, loadState
//
//            let success info = state.Add(pa.Id, (isLoadedInAppDomain, info)), info
//
//            match loadState, pa.StaticInitializer with
//            | NotLoaded _, _ -> success <| writeAssemblyToCache pickler path pa
//            | LoadedWithStaticIntialization(_,info), Some init -> 
//                let info = writeStaticInitializer pickler path (Some info) init
//                success <| LoadedWithStaticIntialization(pa.Id, info)
//
//            | _ when isFirstAccess -> success loadState
//            | _ -> state, loadState
//        
//        with e -> state, LoadFault(pa.Id, e)
//
//
//
//    let initAssemblyCache (pickler : BasePickler) (cacheDir : string) : AssemblyCache =
//        mkStatefulActor Map.empty (fun state (pa,req,policy) -> cachePortableAssembly pickler cacheDir state req policy pa)


//    /// load a portable assembly from cache
//    let tryGetPortableAssemblyFromCache (cache : AssemblyCache) cacheDir includeImage requireIdentical policy (id : AssemblyId) =
//        match cache.PostAndReply (PortableAssembly.Empty id, requireIdentical, policy) with
//        | LoadFault(_,e) -> raise e
//        | NotLoaded _ -> None
//        | loadState ->
//            // check if assembly is recorded as loaded in AppDomain
//            let isLoadedInAppDomain = defaultArg (cache.CurrentState.TryFind id |> Option.map fst) false


    let getCachedAssembly (pickler : BasePickler) cacheDir includeImage (id : AssemblyId) =
        let path = CachePath.Resolve cacheDir id

        let image = 
            if includeImage then
                Some <| File.ReadAllBytes path.Assembly
            else
                None

        let symbols = 
            if includeImage && File.Exists path.Pdb then
                Some <| File.ReadAllBytes path.Pdb
            else
                None

        let staticInit =
            match tryReadMetadata pickler path with
            | Some info ->
                let data = File.ReadAllBytes path.StaticInitializer
                Some { Data = data ; IsPartial = info.IsPartial ; Generation = info.Generation }

            | _ -> None

        { Id = id ; Image = image ; Symbols = symbols ; StaticInitializer = staticInit }