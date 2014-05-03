module internal Nessos.Vagrant.AssemblyCache

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.AssemblyLoader

////
////    type CachedAssemblyInfo =
////        | UnCached
////        | StaticInAppDomainOrGac
////        | CachedStatic
////        | CachedDynamic of requiresStaticInitialization:bool * generation : int option
////    with
////        static member OfDynamicAssemblyInfo(info : DynamicAssemblyInfo) =
////            let gen = info.StaticInitializerData |> Option.map fst
////            CachedDynamic(info.RequiresStaticInitialization, gen)
////
//    type CachePath =
//        {
//            Assembly : string
//            Pdb : string
//            StaticInitializer : string
//            Metadata : string
//        }
//    with
//        /// resolve unique paths for provided assembly in given cache directory
//        static member Resolve (cacheDir : string) (id : AssemblyId) =
//            let hash = Convert.toBase32String id.ImageHash
//            let name = sprintf "%s-%s" (id.GetName().Name) hash
//            let basePath = Path.Combine(cacheDir, name)
//            {
//                Assembly = basePath + ".dll"
//                Pdb = basePath + ".pdb"
//                StaticInitializer = basePath + ".static"
//                Metadata = basePath + ".vagrant"
//            }
//
//    /// write dynamic assembly metadata to cache
//    let writeDynamicAssemblyInfo (pickler : FsPickler) (path : CachePath) info =
//        use fs = new FileStream(path.Metadata, FileMode.Create)
//        pickler.Serialize<DynamicAssemblySliceInfo>(fs, info)
//
//    /// read dynamic assembly metadata from cache
//    let readDynamicAssemblyInfo (pickler : FsPickler) (path : CachePath) =
//        use fs = new FileStream(path.Metadata, FileMode.Open)
//        pickler.Deserialize<DynamicAssemblySliceInfo>(fs)
//
//    let resolveCachedAssemblyInfo (pickler : FsPickler) (cacheDir : string) (id : AssemblyId) =
//        let path = CachePath.Resolve cacheDir id
//
//        let isImageLoaded, isSymbolsLoaded =
//            match tryLoadAssembly id.FullName with
//            | Some a when a.AssemblyId = id ->
//                let pdb = Path.ChangeExtension(a.Location, "pdb")
//                true, File.Exists pdb
//            | _ ->
//                File.Exists path.Assembly, File.Exists path.Pdb
//
//        let sliceInfo =
//            if File.Exists path.Metadata then
//                Some <| readDynamicAssemblyInfo pickler path
//            else
//                None
//
//        { Id = id ; IsImageLoaded = isImageLoaded ; IsSymbolsLoaded = isSymbolsLoaded ; DynamicAssemblySliceInfo = sliceInfo }
//
//
//    /// cache portable assembly image
//    let cachePortableAssembly (pickler : FsPickler) cacheDir (pa : PortableAssembly) =
//        let path = CachePath.Resolve cacheDir pa.Id
//            
//        // cache the file
//        if File.Exists path.Assembly then ()
//        else
//            match pa.Image with
//            | None -> ()
//            | Some img -> File.WriteAllBytes(path.Assembly, img)
//
//        // cache the symbols
//        if File.Exists path.Pdb then ()
//        else
//            match pa.Symbols with
//            | None -> ()
//            | Some symbols -> File.WriteAllBytes(path.Pdb, symbols)
//
//        // cache the static initializer
//        match pa.StaticInitializer with
//        | None -> ()
//        | Some(_,data) -> do File.WriteAllBytes(path.StaticInitializer, data)
//
//        // cache metadata
//        match pa.Info.DynamicAssemblySliceInfo with
//        | None -> ()
//        | Some info -> writeDynamicAssemblyInfo pickler path info
//
//
//    let getCachedAssembly (pickler : FsPickler) (cacheDir : string) (id : AssemblyId) =
//        let path = CachePath.Resolve cacheDir id
//
//        let image = 
//            if File.Exists path.Assembly then
//                Some <| File.ReadAllBytes path.Assembly
//            else
//                None
//
//        let symbols =
//            if File.Exists path.Pdb then
//                Some <| File.ReadAllBytes path.Pdb
//            else
//                None
//
//        let dynAssemblyInfo =
//            if File.Exists path.Metadata then
//                Some <| readDynamicAssemblyInfo pickler path
//            else
//                None
//
//        let staticInitializer =
//            if File.Exists path.StaticInitializer then
//                Some <| File.ReadAllBytes path.StaticInitializer
//            else
//                None
//
//        let info = 
//            { 
//                Id = id ; 
//                IsImageLoaded = image.IsSome ; 
//                IsSymbolsLoaded = symbols.IsSome ;
//                DynamicAssemblySliceInfo = dynAssemblyInfo
//            }
//
//        { Info = info ; Image = image ; Symbols = symbols ; StaticInitializer = staticInitializer }
//                
//
//
//
//
//    let cacheAssembly (pickler : FsPickler) (cacheDir : string) (state : Map<AssemblyId, AssemblyInfo>) (pa : PortableAssembly) =
//
//        let updateWith info = state.Add(pa.Id, info)
//        let path = CachePath.Resolve cacheDir pa.Id
//
//        try        
//            // consult directory state if not found in-memory
//            let state, assemblyInfo = 
//                match state.TryFind pa.Id with
//                | None -> let info = resolveCachedAssemblyInfo pickler cacheDir pa.Id in updateWith info, info
//                | Some info -> state, info
//
//            if not assemblyInfo.IsImageLoaded then
//                cachePortableAssembly pickler cacheDir pa
//                state, LoadSuccess(pa.Id, [||])
//            else
//                let currentGen =
//                    match assemblyInfo.DynamicAssemblySliceInfo with
//                    | None -> -1
//                    | Some info -> info.StaticInitializerGeneration
//
//                match pa.StaticInitializer, pa.Info.DynamicAssemblySliceInfo with
//                | Some(gen,data), Some dyn when gen > currentGen -> 
//                    File.WriteAllBytes(path.StaticInitializer, data)
//                    writeDynamicAssemblyInfo pickler path dyn
//                    updateWith pa.Info, LoadSuccess(pa.Id, [||])
//
//                | _ ->
//                    state, LoadSuccess(pa.Id, [||])
//
//        with e -> state, LoadFault(pa.Id, e)