module internal Nessos.Vagrant.AssemblyCache

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.AssemblyLoader


    type CachedAssemblyInfo =
        | UnCached
        | StaticInAppDomainOrGac
        | CachedStatic
        | CachedDynamic of requiresStaticInitialization:bool * generation : int option
    with
        static member OfDynamicAssemblyInfo(info : DynamicAssemblyInfo) =
            let gen = info.StaticInitializerData |> Option.map fst
            CachedDynamic(info.RequiresStaticInitialization, gen)

    type CachePath =
        {
            Assembly : string
            Pdb : string
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
                Metadata = basePath + ".vagrant"
            }

    /// write dynamic assembly metadata to cache
    let writeDynamicAssemblyInfo (pickler : FsPickler) (path : CachePath) info =
        use fs = new FileStream(path.Metadata, FileMode.Create)
        pickler.Serialize<DynamicAssemblyInfo>(fs, info)

    /// read dynamic assembly metadata from cache
    let readDynamicAssemblyInfo (pickler : FsPickler) (path : CachePath) =
        use fs = new FileStream(path.Metadata, FileMode.Open)
        pickler.Deserialize<DynamicAssemblyInfo>(fs)

    let resolveCachedAssemblyInfo (pickler : FsPickler) (cacheDir : string) (id : AssemblyId) =
        match tryLoadAssembly id.FullName with
        | Some a when a.AssemblyId = id -> StaticInAppDomainOrGac
        | _ ->
            let path = CachePath.Resolve cacheDir id

            if not <| File.Exists path.Assembly then UnCached
            elif File.Exists path.Metadata then
                let info = readDynamicAssemblyInfo pickler path
                CachedAssemblyInfo.OfDynamicAssemblyInfo info
            else
                CachedStatic

    /// cache portable assembly image
    let cachePortableAssembly (pickler : FsPickler) cacheDir (pa : PortableAssembly) =
        let path = CachePath.Resolve cacheDir pa.Id
            
        // cache the file
        if File.Exists path.Assembly then ()
        else
            match pa.Image with
            | None -> invalidOp <| sprintf "AssemblyCache: Portable assembly '%s' lacking image." pa.FullName
            | Some img -> File.WriteAllBytes(path.Assembly, img)

        // cache the symbols
        if File.Exists path.Pdb then ()
        else
            match pa.Symbols with
            | None -> ()
            | Some symbols -> File.WriteAllBytes(path.Pdb, symbols)

        // cache metadata
        match pa.DynamicAssemblyInfo with
        | None -> ()
        | Some info -> writeDynamicAssemblyInfo pickler path info




    let rec cacheAssembly (pickler : FsPickler) (cacheDir : string) (state : Map<AssemblyId, CachedAssemblyInfo>) (pa : PortableAssembly) =

        let updateWith info = state.Add(pa.Id, info)

        let path = CachePath.Resolve cacheDir pa.Id
        
        // consult directory state if not found in-memory
        let state, assemblyInfo = 
            match state.TryFind pa.Id with
            | None -> let info = resolveCachedAssemblyInfo pickler cacheDir pa.Id in updateWith info, info
            | Some info -> state, info

        match assemblyInfo, pa.Image, pa.DynamicAssemblyInfo with
        // assembly not cached and portable assembly does not contain an image
        | UnCached, None, _ -> state, MissingAssemblyImage pa.Id
        // static assembly not cached 
        | UnCached, Some _, None -> 
            do cachePortableAssembly pickler cacheDir pa
            updateWith CachedStatic, Loaded(pa.Id, [||])
        // dynamic assembly not cached
        | UnCached, Some _, Some dyn ->
            do cachePortableAssembly pickler cacheDir pa
            let state = updateWith <| CachedAssemblyInfo.OfDynamicAssemblyInfo dyn
            if dyn.RequiresStaticInitialization && dyn.StaticInitializerData.IsNone then
                state, MissingStaticInitializer(pa.Id, None)
            else
                state, Loaded(pa.Id, [||])

        // static assembly loaded in cache or GAC
        | (CachedStatic | StaticInAppDomainOrGac), _, _ -> state, Loaded(pa.Id, [||])
        // cached dynamic assembly not in need of value initialization
        | CachedDynamic(false, _), _, _ -> state, Loaded(pa.Id, [||])
        // cached dynamic assembly missing any static initialization
        | CachedDynamic(true, None), _, None -> state, MissingStaticInitializer(pa.Id, None)
        | CachedDynamic(true, None), _, Some info ->
            match info.StaticInitializerData with
            | None -> state, MissingStaticInitializer(pa.Id, None)
            | Some(gen,_) ->
                do writeDynamicAssemblyInfo pickler path info 
                state, Loaded(pa.Id, [||])
            
        | CachedDynamic(true, Some gen), _, Some info ->
            match info.StaticInitializerData with
            | None when info.IsPartiallyEvaluated -> state, MissingStaticInitializer(pa.Id, Some gen)
            | None -> state, Loaded(pa.Id, [||])
            | Some(newGen, _) when gen >= newGen -> state, Loaded(pa.Id, [||])
            | Some(newGen, _) ->
                do writeDynamicAssemblyInfo pickler path info
                state, Loaded(pa.Id, [||])

        | CachedDynamic _, _, None -> state, Loaded(pa.Id, [||])