module internal MBrace.Vagabond.DataDependencyManagement

open System
open System.Collections.Generic
open System.Reflection
open System.IO

open MBrace.FsPickler
open MBrace.FsPickler.Hashing

open MBrace.Vagabond.SliceCompilerTypes
open MBrace.Vagabond.AssemblyNaming
open MBrace.Vagabond.AssemblyCache
open MBrace.Vagabond.AssemblyManagementTypes

/// pickle value to file
let picklePersistedBinding (state : VagabondState) (path : string) (value : obj) =
    let result = AssemblyCache.TryOpenWrite path
    match result with
    | None -> ()
    | Some fs ->
        use fs = fs
        let stream = state.Configuration.DataCompressionAlgorithm.Compress fs
        state.Serializer.Serialize(stream, value)

/// unpickle value from file
let unpicklePersistedBinding (state : VagabondState) (path : string) =
    use fs = AssemblyCache.OpenRead path
    let stream = state.Configuration.DataCompressionAlgorithm.Decompress fs
    state.Serializer.Deserialize<obj>(stream)

/// update static binding manifest with new bindings
let updateStaticBindings (bindings : (FieldInfo * HashResult) []) (newBindings : seq<FieldInfo * HashResult>) =
    let dict = new Dictionary<FieldInfo, HashResult> ()
    for f,h in bindings do dict.Add(f,h)
    for f,h in newBindings do dict.[f] <- h
    dict |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toArray

/// export data dependency for locally generated slice
let exportDataDependency (state : VagabondState) (assemblyPath : string) (assemblyId : AssemblyId)
                            (id : DataDependencyId) (current : DataExportState option) (field : FieldInfo) : DataExportState =
    // get current value for static field
    let value = field.GetValue(null)
    // compute the MurMur3 hash for the object; this will allow detection of future mutations of the dependency
    // it also records any exceptions in case of the object being non-serializable.
    let hashResult = try state.Serializer.ComputeHash value |> Choice1Of2 with e -> Choice2Of2 e
    // decide if a new pickle generation needs to be generated
    // compare with previously recorded hash, if it exists.
    let requireNewGen =
        match current, hashResult with
        | None, _ -> true // no previous generation, update
        | Some { Hash = Choice1Of2 hash }, Choice1Of2 hash' -> hash <> hash' // only update if different hash, i.e. value updated
        | Some { Hash = Choice2Of2 _ }, Choice1Of2 _ -> true // if was exception but now successful, update
        | Some { Hash = Choice1Of2 _ }, Choice2Of2 _ -> false // if was successful but is exception, do not update
        | Some { Hash = Choice2Of2 e }, Choice2Of2 e' -> e.GetType() <> e'.GetType() // if was and is exception, only update if different exns

    // no update required, return current state
    if not requireNewGen then Option.get current else

    let data =
        match hashResult with
        // file size exceeds threshold; declare persisted to file
        | Choice1Of2 hash when hash.Length > state.Configuration.DataPersistThreshold -> Persisted hash
        // pickle in metadata
        | Choice1Of2 hash -> let pickle = state.Serializer.PickleTyped value in Pickled (hash, pickle)
        // pickle serialization exception
        | Choice2Of2 e -> Errored (e.ToString(), state.Serializer.PickleTyped e)

    let dependencyInfo =
        match current with
        | None -> { Id = id ; Name = field.ToString() ; Generation = 0 ; FieldInfo = state.Serializer.PickleTyped field ; Data = data }
        | Some { Info = di } -> { di with Generation = di.Generation + 1 ; Data = data }

    // persist to file, if so required
    let persistFile =
        match data with
        | Persisted hash ->
            let persistedPath = state.AssemblyCache.GetPersistedDataPath(assemblyId, hash)
            picklePersistedBinding state persistedPath value

            Some (id, persistedPath)
        | _ -> None

    { AssemblyPath = assemblyPath ; FieldInfo = field ; Hash = hashResult ; Info = dependencyInfo ; PersistFile = persistFile }

/// export data dependencies given controller state for provided compiled dynamic assembly slice
let exportDataDependencies (state : VagabondState) (slice : DynamicAssemblySlice) =
    let assemblyId = slice.Assembly.AssemblyId
    let assemblyPath = slice.Assembly.Location
    let exportState =
        match state.AssemblyLoadState.TryFind assemblyId with
        | None -> slice.StaticFields |> Array.mapi (fun id fI -> exportDataDependency state assemblyPath assemblyId id None fI)
        | Some (ExportedSlice deps) -> slice.StaticFields |> Array.mapi (fun id fI -> exportDataDependency state assemblyPath assemblyId id (Some deps.[id]) fI)
        | Some _ -> invalidOp <| sprintf "internal error: assembly '%s' not local slice." assemblyId.FullName

    let persisted = exportState |> Array.choose (fun de -> de.PersistFile)
    let dependencies = exportState |> Array.map (fun de -> de.Info)
    let va = VagabondAssembly.FromManagedAssembly(slice.Assembly, true, dependencies, persisted)

    { state with 
        AssemblyLoadState = state.AssemblyLoadState.Add(assemblyId, ExportedSlice exportState)
        StaticBindings =
            exportState 
            |> Seq.choose (fun dei -> match dei.Hash with Choice1Of2 h -> Some(dei.FieldInfo, h) | _ -> None)
            |> updateStaticBindings state.StaticBindings
    }, va


/// import data dependency for locally loaded assembly slice
let importDataDependency (state : VagabondState) (va : VagabondAssembly) (current : DataDependencyInfo option)
                            (persistPath : string option) (di : DataDependencyInfo) =

    // determine if data dependency requires updating
    let shouldUpdate =
        match current with
        | None -> true
        | Some di' -> di'.Generation < di.Generation

    // return current generation if update not required
    if not shouldUpdate then None else

    let updateValue hash (value : obj) =
        let field = state.Serializer.UnPickleTyped di.FieldInfo

        // As per https://docs.microsoft.com/en-us/dotnet/api/system.reflection.fieldinfo.setvalue
        // SetValue method cannot be used to set values of static, init-only (readonly in C#) fields reliably. 
        // In .NET Core 3.0 and later versions, an exception is thrown if you attempt to set a value on a static, init-only field.
        if field.Attributes &&& FieldAttributes.Static <> FieldAttributes.Static
            || field.Attributes &&& FieldAttributes.InitOnly <> FieldAttributes.InitOnly then
            field.SetValue(null, value)
        Some(field, hash)

    match di.Data, persistPath with
    // unpickle from metadata
    | Pickled (hash, pickle), _ ->
        let value = state.Serializer.UnPickleTyped pickle
        updateValue hash value
    | Persisted _, None -> invalidOp <| sprintf "Assembly '%O' missing data initializer for value '%s'." va.FullName di.Name
    // unpickle from file
    | Persisted hash, Some path ->
        let value = unpicklePersistedBinding state path
        updateValue hash value
    | Errored _, _ -> None

/// import data dependencies to local AppDomain
let importDataDependencies (state : VagabondState) (current : VagabondAssembly option) (va : VagabondAssembly) =
    if Array.isEmpty va.Metadata.DataDependencies then 
        { state with AssemblyLoadState = state.AssemblyLoadState.Add(va.Id, ImportedSlice va) }
    else
        let persisted = va.PersistedDataDependencies |> Map.ofArray
        let updatedBindings =
            match current with
            | None -> va.Metadata.DataDependencies |> Array.map (fun dd -> importDataDependency state va None (persisted.TryFind dd.Id) dd)
            | Some curr -> 
                (va.Metadata.DataDependencies, curr.Metadata.DataDependencies)
                ||> Array.map2 (fun dd dd' -> importDataDependency state va (Some dd') (persisted.TryFind dd.Id) dd)

        { state with 
            AssemblyLoadState = state.AssemblyLoadState.Add(va.Id, ImportedSlice va) 
            StaticBindings = updateStaticBindings state.StaticBindings (Array.choose id updatedBindings)
        }