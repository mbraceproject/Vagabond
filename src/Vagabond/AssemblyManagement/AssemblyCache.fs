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
    let getPersistedBindingPath (hash : HashResult) = 
        let fileName = DataBinding.CreateUniqueFileNameByHash hash
        Path.Combine(cacheDirectory, fileName + ".dat")

    /// asynchronously writes stream data to file in given path
    static let streamToFile (source : Stream) (path : string) = async {
        use fs = File.OpenWrite path
        do! source.CopyToAsync(fs)
    }

    /// asynchronously writes file data to given stream
    static let fileToStream (path : string) (target : Stream) = async {
        use fs = File.OpenRead path
        do! fs.CopyToAsync(target)
    }

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
    let getPersistedDataDependencies (md : VagabondMetadata) =
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
            PersistedDataDependencies = dataDependencies |> Array.map (fun (dd, hash) -> dd.Id, getPersistedBindingPath hash)
        }

    // Returns persist path for given data dependency
    member __.GetPersistedDataPath (hash : HashResult) =
        getPersistedBindingPath hash

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
        if not <| File.Exists cachePath then
            let! imgReader = importer.GetImageReader id
            do! streamToFile imgReader cachePath
        
        let! symbolsPath = async {
            let symbolsPath = getSymbolsPath cachePath
            if File.Exists symbolsPath then return Some symbolsPath 
            else
                let! symbolsReader = importer.TryGetSymbolReader id
                match symbolsReader with
                | None -> return None
                | Some sReader -> 
                    use sReader = sReader in 
                    do! streamToFile sReader symbolsPath
                    return Some symbolsPath
        }

        // read current metadata, if it exists
        let! metadata = importer.ReadMetadata id
        let persistedDependencies =
            match tryReadMetadata cachePath with
            | None -> getPersistedDataDependencies metadata
            | Some current ->
                // dependencies from previous generations already cached, detect which need updating
                let indexedCurrent = current.DataDependencies |> Seq.map (fun dd -> dd.Id, dd) |> Map.ofSeq
                metadata 
                |> getPersistedDataDependencies
                |> Array.filter (fun (dd,_) -> dd.Generation > indexedCurrent.[dd.Id].Generation)

        let importPersistedDependency (dd : DataDependencyInfo, hash : HashResult) = async {
            let dpath = getPersistedBindingPath hash
            if not <| File.Exists dpath then
                use! dataReader = importer.GetPersistedDataDependencyReader(id, dd)
                do! streamToFile dataReader dpath

            return (dd.Id, dpath)
        }

        // download persisted data dependency files
        let! persistedFiles = persistedDependencies |> Seq.map importPersistedDependency |> Async.Parallel

        // finaly, write metadata file
        writeMetadata cachePath metadata

        return { Id = id ; Image = cachePath ; Symbols = symbolsPath ; Metadata = metadata ; PersistedDataDependencies = persistedFiles }
    }

    /// Exports assembly of given id from cache
    member __.Upload(exporter : IAssemblyUploader, va : VagabondAssembly) = async {
        // 1. Write image if not found in remote party
        let! writer = exporter.TryGetImageWriter va.Id
        match writer with
        | None -> ()
        | Some s ->
            use s = s
            do! fileToStream va.Image s

        // 2. Write symbols if exists locally but not found in remote party
        match va.Symbols with
        | None -> ()
        | Some sf ->
            let! writer = exporter.TryGetSymbolsWriter va.Id
            match writer with
            | None -> ()
            | Some s ->
                use s = s
                do! fileToStream sf s

        // 4. Get remote current metadata, if it exists
        let! remoteMD = exporter.TryGetMetadata va.Id

        // 3. Write data dependencies if not found in remote party
        let localDependencyInfo = va.Metadata.DataDependencies |> Seq.map (fun dd -> dd.Id, dd) |> Map.ofSeq
        let requiredDependencies =
            match remoteMD with
            | None -> va.PersistedDataDependencies
            | Some remoteMD ->
                // dependencies from previous generations already cached, detect which need updating
                let remoteMD = remoteMD.DataDependencies |> Seq.map (fun dd -> dd.Id, dd) |> Map.ofSeq
                va.PersistedDataDependencies
                |> Array.filter (fun (id, _) -> localDependencyInfo.[id].Generation > remoteMD.[id].Generation)

        let writePersistedDataDependency (id : DataDependencyId, path : string) = async {
            use! writer = exporter.GetPersistedDataDependencyReader(va.Id, localDependencyInfo.[id])
            do! fileToStream path writer
        }

        do! requiredDependencies |> Seq.map writePersistedDataDependency |> Async.Parallel |> Async.Ignore

        // 4. Write metadata
        do! exporter.WriteMetadata(va.Id, va.Metadata)
    }