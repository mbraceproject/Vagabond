module internal Nessos.Vagabond.AssemblyCache

open System
open System.IO
open System.Reflection

open Nessos.FsPickler
open Nessos.FsPickler.Hashing

open Nessos.Vagabond
open Nessos.Vagabond.Utils
open Nessos.Vagabond.AssemblyNaming

/// Contains methods for caching assemblies and vagabond metadata to specified folder.
type AssemblyCache (cacheDirectory : string, serializer : FsPicklerSerializer) =
    do
        if not <| Directory.Exists cacheDirectory then
            raise <| new DirectoryNotFoundException(cacheDirectory)

    // gets a unique file name in cache directory that corresponds to assembly id.
    static let getCachedAssemblyPath cacheDirectory (id : AssemblyId) =
        Path.Combine(cacheDirectory, id.GetFileName() + id.Extension)

    /// gets metadata file path for given cached assembly
    static let getMetadataPath path = Path.ChangeExtension(path, ".vgb")

    /// gets file name for persisted data dependency of given hashcode
    let getPersistedDataDependencyPath (id : AssemblyId) (hash : HashResult) = 
        let fileName = DataDependency.CreateUniqueFileNameByHash(hash, prefixId = id)
        Path.Combine(cacheDirectory, fileName + ".dat")

    static let ioRetryPolicy : RetryPolicy =
        function
        | :? IOException, retries when retries < 5 -> Some <| TimeSpan.FromMilliseconds 200.
        | _ -> None

    // resolve symbols file
    static let tryFindSymbols cachePath =
        let symbols = getSymbolsPath cachePath
        if File.Exists symbols then Some symbols
        else None

    /// try reading vagabond metadata from cachePath
    let tryReadMetadata cachePath =
        let metadataPath = getMetadataPath cachePath
        if File.Exists metadataPath then
            use fs = new FileStream(metadataPath, FileMode.Open, FileAccess.Read, FileShare.None)
            let md = serializer.Deserialize<VagabondMetadata>(fs)
            Some md
        else
            None

    /// writes vagabond metadata to cachePath
    let writeMetadata cachePath (md : VagabondMetadata) =
        use fs = new FileStream(getMetadataPath cachePath, FileMode.Create, FileAccess.Write, FileShare.None)
        serializer.Serialize<VagabondMetadata>(fs, md)

    /// read vagabond metadata from cachePath
    let readMetadata cachePath =
        match tryReadMetadata cachePath with
        | None -> raise <| new FileNotFoundException(getMetadataPath cachePath)
        | Some md -> md

    /// returns an array of data dependency paths given metadata
    static let getPersistedDataDependencies (md : VagabondMetadata) =
        md.DataDependencies |> Array.choose (fun dd -> match dd.Data with Persisted hash -> Some (dd, hash) | _ -> None)

    /// gets persisted Vagabond assembly from cache
    let getCachedAssembly (cachePath:string) (id : AssemblyId) =
        let symbols = tryFindSymbols cachePath
        let metadata = readMetadata cachePath
        let dataDependencies = getPersistedDataDependencies metadata
        {
            Id = id
            Image = cachePath
            Symbols = symbols
            Metadata = metadata
            PersistedDataDependencies = dataDependencies |> Array.map (fun (dd, hash) -> dd.Id, getPersistedDataDependencyPath id hash)
        }

    // Returns persist path for given data dependency
    member __.GetPersistedDataPath (id : AssemblyId, hash : HashResult) =
        getPersistedDataDependencyPath id hash

    /// Current cache directory
    member __.CacheDirectory = cacheDirectory

    /// Returns cached assembly of provided id, if it exists
    member __.TryGetCachedAssembly(id : AssemblyId) : VagabondAssembly option =
        let cachePath = getCachedAssemblyPath cacheDirectory id

        if File.Exists cachePath then
            Some <| getCachedAssembly cachePath id
        else
            None

    /// Checks if assembly of provided id exists in cache
    member __.IsCachedAssembly(id : AssemblyId) : bool =
        __.TryGetCachedAssembly(id).IsSome


    /// Asynchronously attempt to acquire a writer file stream to a path that
    /// that does not exist. Returns 'None' if file found to exist.
    static member TryOpenWrite (path : string) =
        // avoid write races by enforcing a retry policy
        Retry.Retry ioRetryPolicy <|
            fun () ->
                if not <| File.Exists path then
                    let fs = new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.None)
                    Some fs
                else
                    None

    /// Asynchronously acquire a reader file stream to a path that should exist.
    static member OpenRead (path : string) =
        Retry.Retry ioRetryPolicy (fun () -> new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read))

    /// Creates a Vagabond assembly record for given System.Reflection.Assembly.
    static member CreateVagabondAssembly(assembly : Assembly, isSlice : bool) : VagabondAssembly =
        if assembly.IsDynamic || String.IsNullOrEmpty assembly.Location then
            invalidArg assembly.FullName "assembly is dynamic or not persistable."
        else
            let location = assembly.Location
            let symbols = tryFindSymbols location
            let metadata =
                {
                    IsManagedAssembly = true
                    IsDynamicAssemblySlice = isSlice
                    DataDependencies = [||]
                }
            {
                Id = assembly.AssemblyId
                Image = location
                Symbols = symbols
                Metadata = metadata
                PersistedDataDependencies = [||]
            }

    /// Import assembly of given id to cache
    member __.Download(importer : IAssemblyDownloader, id : AssemblyId) = async {
        let cachePath = getCachedAssemblyPath cacheDirectory id
        let result = AssemblyCache.TryOpenWrite cachePath
        match result with
        | None -> () // file alreay exists in cache, do not write
        | Some fs ->
            use fs = fs
            use! imgReader = importer.GetImageReader id
            do! imgReader.CopyToAsync fs
        
        let! symbolsPath = async {
            let symbolsPath = getSymbolsPath cachePath
            if File.Exists symbolsPath then return Some symbolsPath 
            else
                let! symbolsReader = importer.TryGetSymbolReader id
                match symbolsReader with
                | None -> return None
                | Some sReader -> 
                    use sReader = sReader
                    let result = AssemblyCache.TryOpenWrite symbolsPath
                    match result with
                    | None -> return None
                    | Some fs ->
                        use fs = fs
                        do! sReader.CopyToAsync fs
                        return Some symbolsPath
        }

        // read current metadata
        let! metadata = importer.ReadMetadata id
        let groupedPersistedDependencies = 
            getPersistedDataDependencies metadata
            |> Seq.groupBy snd
            |> Seq.map (fun (hash,dds) -> dds |> Seq.map fst |> Seq.toArray, hash)
            |> Seq.toArray

        let importPersistedDependency (dds : DataDependencyInfo [], hash : HashResult) = async {
            let dpath = getPersistedDataDependencyPath id hash
            let result = AssemblyCache.TryOpenWrite dpath
            match result with
            | None -> ()
            | Some fs ->
                use fs = fs
                use! dataReader = importer.GetPersistedDataDependencyReader(id, dds.[0], hash)
                do! dataReader.CopyToAsync fs

            return (dds, dpath)
        }

        // download persisted data dependency files
        let! persistedFiles = groupedPersistedDependencies |> Seq.map importPersistedDependency |> Async.Parallel

        // finaly, write metadata file
        writeMetadata cachePath metadata

        return { 
            Id = id
            Image = cachePath
            Symbols = symbolsPath
            Metadata = metadata 
            PersistedDataDependencies = 
                persistedFiles 
                |> Seq.collect (fun (dds,path) -> dds |> Seq.map (fun dd -> dd.Id, path))
                |> Seq.toArray
        }
    }

    /// Exports assembly of given id from cache
    member __.Upload(exporter : IAssemblyUploader, va : VagabondAssembly) = async {
        // 1. Write image if not found in remote party
        let! writer = exporter.TryGetImageWriter va.Id
        match writer with
        | None -> ()
        | Some s ->
            use s = s
            use fs = AssemblyCache.OpenRead va.Image
            do! fs.CopyToAsync s

        // 2. Write symbols if exists locally but not found in remote party
        match va.Symbols with
        | None -> ()
        | Some sf ->
            let! writer = exporter.TryGetSymbolsWriter va.Id
            match writer with
            | None -> ()
            | Some s ->
                use s = s
                use fs = AssemblyCache.OpenRead sf
                do! fs.CopyToAsync s

        // 3. Write data dependencies if not found in remote party
        let groupedPersistedDependencies = 
            getPersistedDataDependencies va.Metadata
            |> Seq.groupBy snd
            |> Seq.map (fun (hash,dds) -> dds |> Seq.head |> fst, hash)
            |> Seq.toArray

        let localPersistedFiles = va.PersistedDataDependencies |> Map.ofArray

        let writePersistedDataDependency (dd : DataDependencyInfo, hash : HashResult) = async {
            let! writerOpt = exporter.TryGetPersistedDataDependencyWriter(va.Id, dd, hash)
            match writerOpt with
            | Some writer ->
                use writer = writer
                let localFile = localPersistedFiles.[dd.Id]
                use fs = AssemblyCache.OpenRead localFile
                do! fs.CopyToAsync writer

            | None -> ()
        }

        do! groupedPersistedDependencies |> Seq.map writePersistedDataDependency |> Async.Parallel |> Async.Ignore

        // 4. Write metadata
        do! exporter.WriteMetadata(va.Id, va.Metadata)
    }