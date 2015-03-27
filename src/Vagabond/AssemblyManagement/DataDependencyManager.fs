module internal Nessos.Vagabond.DataDependencyManagement

open System
open System.Collections.Generic
open System.Reflection
open System.IO
open System.IO.Compression

open Nessos.FsPickler
open Nessos.FsPickler.Hashing

open Nessos.Vagabond.SliceCompilerTypes
open Nessos.Vagabond.AssemblyCache
open Nessos.Vagabond.AssemblyManagementTypes

/// threshold in bytes after which to persist binding to file
let persistThreshold = 10L * 1024L
/// Use Gzip compression for large persisted data bindings
let compress = true

//let exportState = new Dictionary<Assembly * DataDependencyId, DataDependencyInfo * (string * int) option * Choice<HashResult, exn>> ()
//    let importState = new Dictionary<(* assembly path *) string * DataDependencyId, (*generation*) int> ()

let picklePersistedBinding (state : VagabondState) (path : string) (value : obj) =
    use fs = File.OpenWrite path
    let stream =
        if compress then new GZipStream(fs, CompressionMode.Decompress) :> Stream
        else fs :> _
    state.Serializer.Serialize(stream, value)
    stream.Flush()

let unpicklePersistedBinding (state : VagabondState) (path : string) =
    use fs = File.OpenRead path
    let stream =
        if compress then new GZipStream(fs, CompressionLevel.Optimal) :> Stream
        else fs :> _
    state.Serializer.Deserialize<obj>(stream)

/// export data dependency for locally generated slice
let exportDataDependency (state : VagabondState) (assemblyPath : string) 
                            (id : DataDependencyId) (current : DataExportState option) (field : FieldInfo) : DataExportState =
    
    let value = field.GetValue(null)
    let hashResult = try state.Serializer.ComputeHash value |> Choice1Of2 with e -> Choice2Of2 e
    let requireNewGen =
        match current, hashResult with
        | None, _ -> true // no previous gen, update
        | Some{ Hash = Choice1Of2 hash }, Choice1Of2 hash' -> hash <> hash' // only update if different hash, i.e. value updated
        | Some{ Hash = Choice2Of2 _ }, Choice1Of2 _ -> true // if was exception but now successful, update
        | Some{ Hash = Choice1Of2 _ }, Choice2Of2 _ -> false // if was successful but is exception, do not update
        | Some{ Hash = Choice2Of2 e }, Choice2Of2 e' -> e.GetType() <> e'.GetType() // if was and is exception, only update if different exns

    if not requireNewGen then Option.get current else

    let data =
        match hashResult with
        | Choice1Of2 hash when hash.Length > persistThreshold -> Persisted hash.Length
        | Choice1Of2 hash -> let pickle = state.Serializer.PickleTyped value in Pickled pickle
        | Choice2Of2 e -> Errored (e.ToString(), state.Serializer.PickleTyped e)

    let dependencyInfo =
        match current with
        | None -> { Id = id ; Name = field.ToString() ; Generation = 0 ; FieldInfo = state.Serializer.PickleTyped field ; Data = data }
        | Some { Info = di } -> { di with Generation = di.Generation + 1 ; Data = data }

    let persistFile =
        match data with
        | Persisted _ ->
            let persistedPath = AssemblyCache.GetPersistedDataPath(assemblyPath, id, dependencyInfo.Generation)
            picklePersistedBinding state persistedPath value
            Some (id, persistedPath)
        | _ -> None

    { AssemblyPath = assemblyPath ; Hash = hashResult ; Info = dependencyInfo ; PersistFile = persistFile }

let exportDataDependencies (state : VagabondState) (slice : DynamicAssemblySlice) =
    match state.AssemblyExportState.TryFind slice.Assembly.Id with
    | None ->
        let assemblyPath = slice.Assembly.Location
        let dataDependencies = slice.StaticFields |> Array.mapi (fun id fI -> exportDataDependency state assemblyPath id None fI)
        let metadata =
            {
                
            
            }
//
//        exportState.[key] <- (dependencyInfo, persistFile, pickleResult)
//        (dependencyInfo, persistFile)
//
//    /// import data dependency for locally loaded assembly slice
//    let importDataDependency (va : VagabondAssembly) (persistPath : string option) (di : DataDependencyInfo) =
//        let key = va.Image, di.Id
//        let previousGen = importState.TryFind key
//        let shouldUpdate =
//            match previousGen with
//            | None -> true
//            | Some gen -> gen < di.Generation
//
//        if shouldUpdate then
//            let updateValue (value : obj) =
//                let field = serializer.UnPickleTyped di.FieldInfo
//                field.SetValue(null, value)
//                importState.[key] <- di.Generation
//
//            match di.Data, persistPath with
//            | Pickled pickle, _ ->
//                let value = serializer.UnPickleTyped pickle
//                updateValue value
//            | Persisted _, None -> invalidOp <| sprintf "Assembly '%O' missing data initializer for value '%s'." va.FullName di.Name
//            | Persisted _, Some path ->
//                let value = unpicklePersistedBinding path
//                updateValue value
//            | Errored _, _ -> ()  
//
//    member __.ExportDataDependencies(slice : DynamicAssemblySlice) =
//        slice.StaticFields |> Array.mapi (fun id fI -> exportDataDependency slice.Assembly id fI)
//
//    member __.ImportDataDependencies (va : VagabondAssembly) =
//        let indexed = va.PersistedDataDependencies |> Map.ofArray
//        va.Metadata.DataDependencies |> Array.iter (fun di -> importDataDependency va (indexed.TryFind di.Id) di)