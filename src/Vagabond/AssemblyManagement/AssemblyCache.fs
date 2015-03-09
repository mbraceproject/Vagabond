module internal Nessos.Vagabond.AssemblyCache

open System
open System.IO
open System.Reflection

open Nessos.FsPickler

open Nessos.Vagabond
open Nessos.Vagabond.Utils

type StaticInitializer = FieldInfo * obj

type AssemblyCache (cacheDirectory : string, pickler : FsPicklerSerializer) =
    do
        if not <| Directory.Exists cacheDirectory then
            raise <| new DirectoryNotFoundException(cacheDirectory)

    // gets a unique file name in cache directory that corresponds to assembly id.
    let getCachedAssemblyPath (id : AssemblyId) =
        let hash = Convert.toBase32String id.ImageHash
        let name = sprintf "%s-%s" (id.GetName().Name) hash
        Path.Combine(cacheDirectory, name + ".dll")

    static let getMetadataPath path = Path.ChangeExtension(path, ".vagabond")
    static let getSymbolsPath path = 
        if runsOnMono.Value then Path.ChangeExtension(path, ".mdb") 
        else Path.ChangeExtension(path, ".pdb")

    static let getStaticInitPath gen path = sprintf "%s-%d.init" (Path.GetFileNameWithoutExtension path) gen

    static let streamToFile (source : Stream) (path : string) = async {
        use fs = File.OpenWrite path
        do! source.CopyToAsync(fs)
    }

    static let fileToStream (path : string) (target : Stream) = async {
        use fs = File.OpenRead path
        do! fs.CopyToAsync(target)
    }

    // read dynamic assembly metadata from cache
    let tryReadMetadata path =
        let mf = getMetadataPath path
        if File.Exists mf then
            let metadata =
                use fs = File.OpenRead mf
                pickler.Deserialize<VagabondMetadata>(fs)

            let staticInitializer = getStaticInitPath metadata.Generation path
            if File.Exists staticInitializer then
                Some (metadata, staticInitializer)
            else
                raise <| new FileNotFoundException(staticInitializer)
        else
            None
        
    // writes static vagabond metadata based on current state
    let writeMetadata path (input : VagabondMetadata, sourceInit : string) =
        let current = tryReadMetadata path
        match current with
        // input metadata is stale, keep current
        | Some (m, _) when m.Generation > input.Generation -> current
        | _ ->
            let initFile = getStaticInitPath input.Generation path
            File.Copy(sourceInit, initFile, true)
            do
                use fs = File.OpenWrite (getMetadataPath path)
                pickler.Serialize<VagabondMetadata>(fs, input)

            Some(input, initFile)

    // resolve symbols file
    let tryFindSymbols path =
        let symbols = getSymbolsPath path
        if File.Exists symbols then Some symbols
        else None

    let getPersistedAssemblyInfo (path : string) (id : AssemblyId) =
        let symbols = tryFindSymbols path
        let metadata = tryReadMetadata path
        {
            Id = id
            Image = path
            Symbols = symbols
            Metadata = metadata
        }

    member __.CacheDirectory = cacheDirectory

    member __.TryGetCachedAssemblyInfo(id : AssemblyId) =
        let cachePath = getCachedAssemblyPath id

        if File.Exists cachePath then
            Some <| getPersistedAssemblyInfo cachePath id
        else
            None

    member __.IsCachedAssembly(id : AssemblyId) =
        __.TryGetCachedAssemblyInfo(id).IsSome

    member __.CreateAssemblyPackage(assembly : Assembly) =
        if assembly.IsDynamic || String.IsNullOrEmpty assembly.Location then
            invalidArg assembly.FullName "assembly is dynamic or not persistable."
        else
            getPersistedAssemblyInfo assembly.Location assembly.AssemblyId

    member __.WriteStaticInitializers(assembly : Assembly, initializers : StaticInitializer [], metadata : VagabondMetadata) =
        if assembly.IsDynamic || String.IsNullOrEmpty assembly.Location then
            invalidArg assembly.FullName "assembly is dynamic or not persistable."
        else
            let symbols = tryFindSymbols assembly.Location
            let initFile = getStaticInitPath metadata.Generation assembly.Location
            use fs = File.OpenWrite initFile
            pickler.Serialize(fs, initializers)
            { Id = assembly.AssemblyId ; Image = assembly.Location ; Symbols = symbols ; Metadata = Some(metadata, initFile) }

    member __.TryReadStaticInitializers(pkg : AssemblyPackage) =
        match pkg.Metadata with
        | Some(_,initFile) ->
            use fs = File.OpenRead initFile
            let init = pickler.Deserialize<StaticInitializer []>(fs)
            Some init
        | None -> None

    member __.Import(importer : IAssemblyImporter, id : AssemblyId) = async {
        let cachePath = getCachedAssemblyPath id
        do! async {
            use! imgReader = importer.GetImageReader id
            do! streamToFile imgReader cachePath
        }

        let! symbolsReader = importer.TryGetSymbolReader id
        match symbolsReader with
        | None -> ()
        | Some sReader -> use sReader = sReader in do! streamToFile sReader (getSymbolsPath cachePath)

        let! metadata = importer.TryReadMetadata id
        match metadata with
        | None -> ()
        | Some md ->
            let metadataFile = getSymbolsPath cachePath
            do! async {
                use! dataReader = importer.GetDataReader id
                do! streamToFile dataReader metadataFile
            }

            let _ = writeMetadata cachePath (md, metadataFile)
            ()
    }

    member __.Export(exporter : IAssemblyExporter, pkg : AssemblyPackage) = async {
        do! async {
            use! writer = exporter.GetImageWriter(pkg.Id)
            do! fileToStream pkg.Image writer
        }

        match pkg.Symbols with
        | None -> ()
        | Some sf ->
            use! writer = exporter.GetSymbolWriter pkg.Id
            do! fileToStream sf writer

        match pkg.Metadata with
        | None -> ()
        | Some (md, init) ->
            do! async {
                use! writer = exporter.GetDataWriter pkg.Id
                do! fileToStream init writer
            }

            do! exporter.WriteMetadata(pkg.Id, md)
    }






//        do File.Copy(pa.Image, cachePath, true)
//        match pa.Symbols with
//        | None -> ()
//        | Some sp -> File.Copy(sp, Path.ChangeExtension(cachePath, symbolsExt), true)
//
//        match pa.Metadata with
//        | None -> ()
//        | Some mp -> writeMetadata cachePath mp |> ignore
//        let cachePath = getCachedAssemblyPath pa.Id
//        if File.Exists cachePath then
//            let info = getPersistedAssemblyInfo cachePath pa.Id
//            let staticInit =
//                match pa.StaticInitializer with
//                | None -> info.StaticInitializer
//                | Some init -> writeStaticInitializer cachePath info.StaticInitializer init
//
//            { info with StaticInitializer = staticInit }
//
//        else
//            match pa.Image with
//            | None -> 
//                let msg = sprintf "Assembly package '%O' lacking image specification." pa.FullName
//                raise <| new VagabondException(msg)
//
//            | Some img -> 
//                File.WriteAllBytes(cachePath, img)
//
//                let symbols =
//                    match pa.Symbols with
//                    | None -> None
//                    | Some sym -> 
//                        let symFile = Path.ChangeExtension(cachePath, symbolsExt)
//                        File.WriteAllBytes(symFile, sym)
//                        Some symFile
//
//                let staticInit =
//                    match pa.StaticInitializer with
//                    | None -> None
//                    | Some init -> writeStaticInitializer cachePath None init
//
//                { Id = pa.Id ; Location = cachePath ; Symbols = symbols ; StaticInitializer = staticInit }


//    member __.Cache(assembly : Assembly, ?asId : AssemblyId) =
//        let info = __.GetStaticAssemblyInfo assembly
//        let id = defaultArg asId info.Id
//        let cachePath = getCachedAssemblyPath id
//        if File.Exists cachePath then
//            getPersistedAssemblyInfo cachePath id
//        else
//            File.Copy(assembly.Location, cachePath)
//            let symbols =
//                match info.Symbols with
//                | None -> None
//                | Some s ->
//                    let symFile = Path.ChangeExtension(cachePath, symbolsExt)
//                    File.Copy(s, symFile)
//                    Some symFile
//
//            { Id = id ; Location = cachePath ; Symbols = symbols ; StaticInitializer = None }