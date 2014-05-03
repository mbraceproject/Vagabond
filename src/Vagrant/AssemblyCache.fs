module internal Nessos.Vagrant.AssemblyCache

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.AssemblyLoader

    type AssemblyCache = StatefulActor<Map<AssemblyId, AssemblyLoadInfo>, PortableAssembly, AssemblyLoadInfo>

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
    let writeMetadata (pickler : FsPickler) (path : CachePath) info =
        use fs = new FileStream(path.Metadata, FileMode.Create)
        pickler.Serialize<StaticInitializationInfo>(fs, info)

    /// read dynamic assembly metadata from cache
    let readMetadata (pickler : FsPickler) (path : CachePath) =
        use fs = new FileStream(path.Metadata, FileMode.Open)
        pickler.Deserialize<StaticInitializationInfo>(fs)

    /// query cache dir for current state
    let resolveCachedAssemblyInfo (pickler : FsPickler) (path : CachePath) (id : AssemblyId) =
        try
            if not <| File.Exists path.Assembly then NotLoaded id
            elif File.Exists path.Metadata then
                let info = readMetadata pickler path
                if File.Exists path.StaticInitializer then
                    LoadedWithStaticIntialization (id, info)
                else
                    let msg = sprintf "cache error: missing static initialization file for assembly '%s'" id.FullName
                    LoadFault(id, VagrantException(msg))
            else
                Loaded id

        with e -> LoadFault(id, e)

    /// write new static initializer to cache
    let writeStaticInitializer (pickler : FsPickler) (path : CachePath) (previous : StaticInitializationInfo option) (init : StaticInitializer) =
        match previous with
        | Some p when p.Generation > init.Generation -> p
        | _ ->
            let info = { Generation = init.Generation ; Errors = [||] ; IsPartial = init.IsPartial }
            writeMetadata pickler path info
            File.WriteAllBytes(path.StaticInitializer, init.Data)
            info

    /// write portable assembly to cache
    let writeAssemblyToCache (pickler : FsPickler) (path : CachePath) (pa : PortableAssembly) =
        match pa.Image with
        | None -> NotLoaded pa.Id
        | Some img -> 

            do File.WriteAllBytes(path.Assembly, img)

            match pa.Symbols with
            | None -> ()
            | Some symbols -> File.WriteAllBytes(path.Pdb, symbols)

            // cache the static initializer
            match pa.StaticInitializer with
            | None -> Loaded pa.Id
            | Some init -> 
                let info = writeStaticInitializer pickler path None init
                LoadedWithStaticIntialization(pa.Id, info)

    /// the main portable assembly method
    let cachePortableAssembly (pickler : FsPickler) (cacheDir : string) (state : Map<AssemblyId, AssemblyLoadInfo>) (pa : PortableAssembly) =
        
        let success info = state.Add(pa.Id, info), info
        let error e = state, LoadFault(pa.Id, e)

        try
            let path = CachePath.Resolve cacheDir pa.Id

            let loadState =
                match state.TryFind pa.Id with
                | None -> resolveCachedAssemblyInfo pickler path pa.Id
                | Some s -> s

            match loadState, pa.StaticInitializer with
            | NotLoaded _, _ -> success <| writeAssemblyToCache pickler path pa
            | LoadedWithStaticIntialization(_,info), Some init -> 
                let info = writeStaticInitializer pickler path (Some info) init
                success <| LoadedWithStaticIntialization(pa.Id, info)

            | _ -> state, loadState
        
        with e -> state, LoadFault(pa.Id, e)



    let initAssemblyCache (pickler : FsPickler) (cacheDir : string) : AssemblyCache =
        mkStatefulActor Map.empty (cachePortableAssembly pickler cacheDir)


    /// load a portable assembly from cache
    let tryGetPortableAssemblyFromCache (cache : AssemblyCache) cacheDir includeImage (id : AssemblyId) =
        match cache.PostAndReply <| PortableAssembly.Empty id with
        | LoadFault(_,e) -> raise e
        | NotLoaded _ -> None
        | loadState ->
            let path = CachePath.Resolve cacheDir id
            let image, symbols =
                if includeImage then
                    let image = Some <| File.ReadAllBytes path.Assembly
                    let symbols =
                        if File.Exists path.Pdb then
                            Some <| File.ReadAllBytes path.Pdb
                        else
                            None

                    image, symbols
                else
                    None, None

            let staticInit =
                match loadState with
                | LoadedWithStaticIntialization (_,info) ->
                    let data = File.ReadAllBytes path.StaticInitializer
                    Some { Data = data ; IsPartial = info.IsPartial ; Generation = info.Generation }

                | _ -> None

            Some { Id = id ; Image = image ; Symbols = symbols ; StaticInitializer = staticInit }