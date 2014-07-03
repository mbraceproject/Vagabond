module internal Nessos.Vagrant.AssemblyCache

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils

    type CachedAssemblyInfo =
        {
            Id : AssemblyId
            Location : string
            Symbols : string option
            StaticInitializer : (string * StaticInitializationInfo) option
        }

    type AssemblyCache (cacheDirectory : string, pickler : BasePickler) =
        do
            if not <| Directory.Exists cacheDirectory then
                raise <| new DirectoryNotFoundException(cacheDirectory)

        static let metadataExt = ".vagrant"
        static let staticInitExt = ".init"
        static let symbolsExt = if runsOnMono.Value then ".mdb" else ".pdb"

        // gets a unique file name in cache directory that corresponds to assembly id.
        let getCachedAssemblyPath (id : AssemblyId) =
            let hash = Convert.toBase32String id.ImageHash
            let name = sprintf "%s-%s" (id.GetName().Name) hash
            Path.Combine(cacheDirectory, name + ".dll")

        // write dynamic assembly metadata to cache
        let writeMetadata path info =
            use fs = new FileStream(Path.ChangeExtension(path, metadataExt), FileMode.Create)
            pickler.Serialize<StaticInitializationInfo>(fs, info)
        
        // writes static initializatin data based on current state
        let writeStaticInitializer path (previous : (string * StaticInitializationInfo) option) (init : StaticInitializer) =
            match previous with
            | Some (_,p) when p.Generation > init.Generation -> previous
            | _ ->
                let info = { Generation = init.Generation ; Errors = [||] ; IsPartial = init.IsPartial }
                writeMetadata path info
                let initFile = Path.ChangeExtension(path, staticInitExt)
                File.WriteAllBytes(initFile, init.Data)
                Some(initFile, info)

        // read dynamic assembly metadata from cache
        let tryReadMetadata path =
            let mf = Path.ChangeExtension(path, metadataExt)
            if File.Exists mf then
                use fs = new FileStream(mf, FileMode.Open)
                let metadata = pickler.Deserialize<StaticInitializationInfo>(fs)
                let staticInitializer = Path.ChangeExtension(path, staticInitExt)
                if File.Exists staticInitializer then
                    Some (staticInitializer, metadata)
                else
                    None
            else
                None

        // resolve symbols file
        let tryFindSymbols path =
            let s1 = Path.ChangeExtension(path, ".pdb")
            if File.Exists s1 then Some s1
            else
                let s2 = Path.ChangeExtension(path, ".mdb")
                if File.Exists s2 then Some s2
                else None

        let getPersistedAssemblyInfo (path : string) (id : AssemblyId) =
            let symbols = tryFindSymbols path
            let metadata = tryReadMetadata path

            {
                Id = id
                Location = path
                Symbols = symbols
                StaticInitializer = metadata
            }


        member __.TryGetCachedAssemblyInfo(id : AssemblyId) =
            let cachePath = getCachedAssemblyPath id

            if File.Exists cachePath then
                Some <| getPersistedAssemblyInfo cachePath id
            else
                None

        member __.IsCachedAssembly(id : AssemblyId) =
            __.TryGetCachedAssemblyInfo(id).IsSome

        member __.GetStaticAssemblyInfo(assembly : Assembly) =
            if assembly.IsDynamic || String.IsNullOrEmpty assembly.Location then
                invalidArg assembly.FullName "assembly is dynamic or not persistable."
            else
                getPersistedAssemblyInfo assembly.Location assembly.AssemblyIdss

        member __.CreatePortableAssembly(cai : CachedAssemblyInfo, includeImage : bool) =

            let image =
                if includeImage then Some <| File.ReadAllBytes cai.Location
                else
                    None

            let symbols = cai.Symbols |> Option.map File.ReadAllBytes

            let staticInit =
                match cai.StaticInitializer with
                | None -> None
                | Some(path, info) ->
                    let data = File.ReadAllBytes path
                    Some {
                        Generation = info.Generation
                        Data = data
                        IsPartial = info.IsPartial
                    }

            {
                Id = cai.Id
                Image = image
                Symbols = symbols
                StaticInitializer = staticInit
            }

        member __.CreatePortableAssembly(assembly : Assembly, includeImage) =
            let info = __.GetStaticAssemblyInfo assembly
            __.CreatePortableAssembly(info, includeImage)

        member __.Cache(pa : PortableAssembly) =
            let cachePath = getCachedAssemblyPath pa.Id
            if File.Exists cachePath then
                let info = getPersistedAssemblyInfo cachePath pa.Id
                let staticInit =
                    match pa.StaticInitializer with
                    | None -> None
                    | Some init -> writeStaticInitializer cachePath info.StaticInitializer init

                { info with StaticInitializer = staticInit }

            else
                match pa.Image with
                | None -> 
                    let msg = sprintf "Portable assembly '%O' lacking image specification." pa.FullName
                    raise <| new VagrantException(msg)

                | Some img -> 
                    File.WriteAllBytes(cachePath, img)

                    let symbols =
                        match pa.Symbols with
                        | None -> None
                        | Some sym -> 
                            let symFile = Path.ChangeExtension(cachePath, symbolsExt)
                            File.WriteAllBytes(symFile, sym)
                            Some symFile

                    let staticInit =
                        match pa.StaticInitializer with
                        | None -> None
                        | Some init -> writeStaticInitializer cachePath None init

                    { Id = pa.Id ; Location = cachePath ; Symbols = symbols ; StaticInitializer = staticInit }


        member __.Cache(assembly : Assembly, ?asId : AssemblyId) =
            let info = __.GetStaticAssemblyInfo assembly
            let id = defaultArg asId info.Id
            let cachePath = getCachedAssemblyPath id
            if File.Exists cachePath then
                getPersistedAssemblyInfo cachePath id
            else
                File.Copy(assembly.Location, cachePath)
                let symbols =
                    match info.Symbols with
                    | None -> None
                    | Some s ->
                        let symFile = Path.ChangeExtension(cachePath, symbolsExt)
                        File.Copy(s, symFile)
                        Some symFile

                { Id = id ; Location = cachePath ; Symbols = symbols ; StaticInitializer = None }
                    
//                    match pa.StaticInitializer, info.StaticInitializer with
//                    | None, init -> init
//                    | Some data, Some(path, info) -> writeStaticInitializer cachePath (Some info) data
//                    | 
//
//            if File.Exists cachePath then
//                match pa.StaticInitializer with
//                | Some init ->
//                    writeStaticInitializer
                
            

//    let getAssemblyCacheState (pickler : BasePickler) (path : CachePath) (id : AssemblyId) =
//        try
//            if not <| File.Exists path.Assembly then NotLoaded id
//            else
//                match tryReadMetadata pickler path with
//                | Some info when File.Exists path.StaticInitializer -> Loaded(id, false, Some info)
//                | Some _ ->
//                    let msg = sprintf "cache error: missing static initialization file for assembly '%s'" id.FullName
//                    LoadFault(id, VagrantException(msg))
//
//                | None -> Loaded (id, false, None)
//
//        with e -> LoadFault(id, e)
//
//    /// write new static initializer to cache
//    let writeStaticInitializer (pickler : BasePickler) (path : CachePath) (previous : StaticInitializationInfo option) (init : StaticInitializer) =
//        match previous with
//        | Some p when p.Generation > init.Generation -> p
//        | _ ->
//            let info = { Generation = init.Generation ; Errors = [||] ; IsPartial = init.IsPartial }
//            writeMetadata pickler path info
//            File.WriteAllBytes(path.StaticInitializer, init.Data)
//            info
//
//    /// write portable assembly to cache
//    let cachePortableAssembly (pickler : BasePickler) (path : CachePath) (pa : PortableAssembly) =
//        try
//            match getAssemblyCacheState pickler path pa.Id with
//            | Loaded(staticInitialization = None) as l -> l
//            | Loaded(staticInitialization = Some info) as l ->
//                match pa.StaticInitializer with
//                | Some init -> 
//                    let info' = writeStaticInitializer pickler path (Some info) init
//                    Loaded(pa.Id, false, Some info')
//                | None -> l
//
//            | _ ->
//
//            match pa.Image with
//            | None -> NotLoaded pa.Id
//            | Some img ->
//
//                do File.WriteAllBytes(path.Assembly, img)
//
//                match pa.Symbols with
//                | None -> ()
//                | Some symbols -> File.WriteAllBytes(path.Pdb, symbols)
//
//                // cache the static initializer
//                match pa.StaticInitializer with
//                | None -> Loaded (pa.Id, false, None)
//                | Some init -> 
//                    let info = writeStaticInitializer pickler path None init
//                    Loaded(pa.Id, false, Some info)
//
//        with e -> LoadFault(pa.Id, e)

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

//
//    let getCachedAssembly (pickler : BasePickler) cacheDir includeImage (id : AssemblyId) =
//        let path = CachePath.Resolve cacheDir id
//
//        let image = 
//            if includeImage then
//                Some <| File.ReadAllBytes path.Assembly
//            else
//                None
//
//        let symbols = 
//            if includeImage && File.Exists path.Pdb then
//                Some <| File.ReadAllBytes path.Pdb
//            else
//                None
//
//        let staticInit =
//            match tryReadMetadata pickler path with
//            | Some info ->
//                let data = File.ReadAllBytes path.StaticInitializer
//                Some { Data = data ; IsPartial = info.IsPartial ; Generation = info.Generation }
//
//            | _ -> None
//
//        { Id = id ; Image = image ; Symbols = symbols ; StaticInitializer = staticInit }