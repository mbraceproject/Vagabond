namespace Nessos.Vagabond
    
open System
open System.IO
open System.Reflection

open Nessos.FsPickler
open Nessos.FsPickler.Hashing

open Nessos.Vagabond.Utils
open Nessos.Vagabond.AssemblyNaming
open Nessos.Vagabond.DependencyAnalysis
open Nessos.Vagabond.Control

/// Vagabond management object which instantiates a dynamic assembly compiler, loader and exporter states
[<AutoSerializable(false)>]
type VagabondManager internal (?cacheDirectory : string, ?profiles : IDynamicAssemblyProfile list, ?typeConverter : ITypeNameConverter, 
                                ?isIgnoredAssembly : Assembly -> bool, ?requireLoadedInAppDomain, ?loadPolicy : AssemblyLoadPolicy,
                                ?compressDataFiles : bool, ?dataPersistTreshold : int64) =

    static do registerAssemblyResolutionHandler ()

    let uuid = Guid.NewGuid()
    let cacheDirectory = 
        match cacheDirectory with 
        | Some d when Directory.Exists d -> d
        | Some d -> raise <| new DirectoryNotFoundException(d)
        | None -> 
            let subdir = sprintf "vagabond-%s" <| uuid.ToString("N")
            let path = Path.Combine(Path.GetTempPath(), subdir)
            let _ = Directory.CreateDirectory(path)
            path

    let isIgnoredAssembly = defaultArg isIgnoredAssembly (fun _ -> false)
    let requireLoadedInAppDomain = defaultArg requireLoadedInAppDomain true
    let compressDataFiles = defaultArg compressDataFiles true
    let dataPersistTreshold = defaultArg dataPersistTreshold (10L * 1024L)

    let profiles =
        match profiles with
        | Some ps -> ps
        | None -> [ new FsiDynamicAssemblyProfile() :> IDynamicAssemblyProfile ]

    let mutable _loadPolicy = defaultArg loadPolicy <| AssemblyLoadPolicy.ResolveStrongNames

    let controller = new VagabondController(uuid, cacheDirectory, profiles, requireLoadedInAppDomain, compressDataFiles, 
                                                        dataPersistTreshold, isIgnoredAssembly, ?tyConv = typeConverter)
    do controller.Start()

    static let checkIsDynamic(a : Assembly) = 
        if a.IsDynamic then () 
        else invalidArg a.FullName "Vagabond: not a dynamic assembly"

    let compile (assemblies : Assembly list) = 
        controller.PostAndReply(fun ch -> CompileDynamicAssemblySlice(assemblies, ch))

    /// Unique identifier for the slice compiler
    member __.UUId = controller.CompilerState.CompilerId
    /// Returns the pickler used by the slice compiler
    member __.Pickler = controller.DefaultPickler
    /// FsPickler type name converter for use with other formats
    member __.TypeConverter = controller.TypeNameConverter
    /// Cache directory used by Vagabond
    member __.CachePath = controller.CacheDirectory

    /// Gets or sets the default load policy for the instance
    member __.DefaultLoadPolicy
        with get () = _loadPolicy
        and set p = _loadPolicy <- p


    /// <summary>
    ///     Includes an unmanaged assembly dependency to the vagabond instance.
    /// </summary>
    /// <param name="path">Path to the unmanaged assembly.</param>
    /// <param name="name">Identifier for unmanaged assembly. Defaults to the assembly file name.</param>
    [<CompilerMessage("Native assembly support is an experimental feature of Vagabond.", 1571)>]
    member __.RegisterNativeDependency(path : string) : VagabondAssembly =
        if runsOnMono.Value then raise <| new PlatformNotSupportedException("Native dependencies not supported on mono.")
        let va = VagabondAssembly.CreateUnmanaged(path)
        controller.PostAndReply(fun ch -> RegisterNativeDependency(va, ch))
        va

    /// Returns all unmanaged dependencies registered to Vagabond instance.
    member __.NativeDependencies : VagabondAssembly list = controller.NativeDependencies

    //
    //  #region Assembly compilation
    //

    /// <summary>
    ///     Compiles slices for given dynamic assembly, if required.
    /// </summary>
    /// <param name="assembly">a dynamic assembly</param>
    member __.CompileDynamicAssemblySlice (assembly : Assembly) : Assembly list =
        do checkIsDynamic assembly
        compile [assembly] |> List.map (fun a -> a.Assembly)

    /// <summary>
    ///     Returns a list of dynamic assemblies that require slice compilation
    ///     for the given object graph to be exportable.
    /// </summary>
    /// <param name="obj">any object graph</param>
    member __.ResolveDynamicDependenciesRequiringCompilation(obj : obj) : Assembly list =
        let dependencies = computeDependencies obj
        getDynamicDependenciesRequiringCompilation controller.CompilerState dependencies

    /// <summary>
    ///     Returns *all* assembly slices of given dynamic assembly.
    /// </summary>
    /// <param name="assembly">a dynamic assembly.</param>
    member __.GetDynamicAssemblySlices(assembly : Assembly) : Assembly list =
        do checkIsDynamic assembly
        match controller.CompilerState.DynamicAssemblies.TryFind assembly.FullName with
        | None -> []
        | Some info -> info.GeneratedSlices |> Seq.map (function KeyValue(_,s) -> s.Assembly) |> Seq.toList

    /// <summary>
    ///     Returns a collection of all assemblies that the given object depends on.
    ///     Dynamic assemblies are substituted for their corresponding static slices.
    /// </summary>
    /// <param name="obj">A given object graph</param>
    /// <param name="permitCompilation">Compile new slices as required. Defaults to false.</param>
    member __.ComputeObjectDependencies(obj : obj, ?permitCompilation : bool) : Assembly list =
        let allowCompilation = defaultArg permitCompilation false

        let dependencies = computeDependencies obj

        if allowCompilation then
            let assemblies = getDynamicDependenciesRequiringCompilation controller.CompilerState dependencies
            let _ = compile assemblies in ()

        remapDependencies isIgnoredAssembly requireLoadedInAppDomain controller.CompilerState dependencies

    /// <summary>
    ///     Checks if assembly id is a locally generated dynamic assembly slice.
    /// </summary>
    /// <param name="id">input assembly id.</param>
    member __.IsLocalDynamicAssemblySlice(id : AssemblyId) : bool =
        controller.CompilerState.IsLocalDynamicAssemblySlice id

    /// <summary>
    ///     Returns the dynamic assembly slice corresponding to the given type, if exists.
    /// </summary>
    /// <param name="t">input type.</param>
    member __.TryGetSliceOfType(t : Type) : Assembly option =
        let t = if t.IsGenericType && not t.IsGenericTypeDefinition then t.GetGenericTypeDefinition() else t
        match controller.CompilerState.DynamicAssemblies.TryFind t.Assembly.FullName with
        | None -> None
        | Some dyn -> dyn.TryGetSlice t |> Option.map (fun s -> s.Assembly)

        
    /// <summary>
    ///     Gets vagabond assembly metadata for given assembly id.
    /// </summary>
    /// <param name="id">assembly id</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to strong names only.</param>
    member __.GetVagabondAssembly(id : AssemblyId, ?loadPolicy : AssemblyLoadPolicy) : VagabondAssembly =
        let loadPolicy = defaultArg loadPolicy _loadPolicy
        controller.PostAndReply(fun ch -> GetVagabondAssembly(loadPolicy, id, ch))

    //
    //  #region Vagabond Assemblies
    //

    /// <summary>
    ///    Gets vagabond assembly metadata for given assembly ids.
    /// </summary>
    /// <param name="ids">assembly ids.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.GetVagabondAssemblies(ids : seq<AssemblyId>, ?loadPolicy : AssemblyLoadPolicy) : VagabondAssembly list =
        ids
        |> Seq.map (fun id -> __.GetVagabondAssembly(id, ?loadPolicy = loadPolicy))
        |> Seq.toList
            

    /// <summary>
    ///     Gets vagabond assembly metadata for given static assembly.
    /// </summary>
    /// <param name="assembly">Given assembly.</param>
    member __.GetVagabondAssembly(assembly : Assembly) : VagabondAssembly =
        __.GetVagabondAssembly(assembly.AssemblyId)

    /// <summary>
    ///     Gets vagabond assembly metadata for given static assemblies.
    /// </summary>
    /// <param name="assemblies">Inputs assemblies.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.GetVagabondAssemblies(assemblies : seq<Assembly>, ?loadPolicy : AssemblyLoadPolicy) : VagabondAssembly list =
        assemblies
        |> Seq.map (fun asm -> __.GetVagabondAssembly(asm.AssemblyId, ?loadPolicy = loadPolicy))
        |> Seq.toList


    /// <summary>
    ///     Gets the local assembly load info for given assembly id.
    /// </summary>
    /// <param name="id">Given assembly id.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.GetAssemblyLoadInfo(id : AssemblyId, ?loadPolicy : AssemblyLoadPolicy) =
        let loadPolicy = defaultArg loadPolicy _loadPolicy
        controller.PostAndReply(fun ch -> GetAssemblyLoadInfo(loadPolicy, id, ch))

    /// <summary>
    ///     Gets the local assembly load info for given assembly ids.
    /// </summary>
    /// <param name="ids">Given assembly ids.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.GetAssemblyLoadInfo(ids : seq<AssemblyId>, ?loadPolicy : AssemblyLoadPolicy) : AssemblyLoadInfo list =
        ids
        |> Seq.map (fun id -> __.GetAssemblyLoadInfo(id, ?loadPolicy = loadPolicy))
        |> Seq.toList

    //
    //  #region Asssembly loading API
    //

    /// <summary>
    ///     Loads vagabond assembly to the local AppDomain.
    /// </summary>
    /// <param name="va">Input assembly package.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.LoadVagabondAssembly(va : VagabondAssembly, ?loadPolicy : AssemblyLoadPolicy) : AssemblyLoadInfo =
        let loadPolicy = defaultArg loadPolicy _loadPolicy
        controller.PostAndReply(fun ch -> LoadAssembly(loadPolicy, va, ch))

    /// <summary>
    ///     Loads vagabond assemblies to the local AppDomain.
    /// </summary>
    /// <param name="vas">Input assembly packages.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.LoadVagabondAssemblies(vas : seq<VagabondAssembly>, ?loadPolicy : AssemblyLoadPolicy) : AssemblyLoadInfo list =
        vas
        |> Seq.map (fun va -> __.LoadVagabondAssembly(va, ?loadPolicy = loadPolicy))
        |> Seq.toList

    /// <summary>
    ///     Attempt loading vagabond assembly of given id to the local AppDomain.
    /// </summary>
    /// <param name="id">input assembly id.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.LoadVagabondAssembly(id : AssemblyId, ?loadPolicy : AssemblyLoadPolicy) : AssemblyLoadInfo =
        match controller.AssemblyCache.TryGetCachedAssembly id with
        | None -> __.GetAssemblyLoadInfo(id, ?loadPolicy = loadPolicy)
        | Some va -> __.LoadVagabondAssembly(va, ?loadPolicy = loadPolicy)

    /// <summary>
    ///     Attempt loading vagabond assemblies of given id's to the local AppDomain.
    /// </summary>
    /// <param name="id">input assembly id.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.LoadVagabondAssemblies(ids : seq<AssemblyId>, ?loadPolicy : AssemblyLoadPolicy) : AssemblyLoadInfo list =
        ids
        |> Seq.map (fun id -> __.LoadVagabondAssembly(id, ?loadPolicy = loadPolicy))
        |> Seq.toList

    //
    // #region Assembly import/export API
    //

    /// <summary>
    ///     Export provided assemblies using provided exporter
    /// </summary>
    /// <param name="exporter">Assembly exporter implementation.</param>
    /// <param name="assemblies">Assemblies to be exported.</param>
    member __.ExportAssemblies(exporter : IAssemblyExporter, assemblies : seq<VagabondAssembly>) : Async<unit> = async {
        return! controller.PostAndAsyncReply(fun ch -> ExportAssemblies(exporter, Seq.toList assemblies, ch))
    }

    /// <summary>
    ///     Import provided assemblies to cache using provided importer.
    /// </summary>
    /// <param name="importer">Assembly importer implementation.</param>
    /// <param name="ids">Assembly id's to be imported.</param>
    member __.ImportAssemblies(importer : IAssemblyImporter, ids : seq<AssemblyId>) : Async<VagabondAssembly list> = async {
        return! controller.PostAndAsyncReply(fun ch -> ImportAssemblies(importer, Seq.toList ids, ch))
    }

    /// Gets hash information for all Vagabond managed static bindings in current AppDomain.
    member __.StaticBindings : (FieldInfo * HashResult) [] = controller.StaticBindings

    /// <summary>
    ///     Try get a Vagabond static bindings that matches provided object hash.
    /// </summary>
    /// <param name="hash">Input object hash.</param>
    member __.TryGetBindingByHash(hash : HashResult) : FieldInfo option =
        match controller.StaticBindings |> Array.tryFind (fun (_,h) -> h = hash) with
        | None -> None
        | Some(f,_) -> Some f


/// <summary>
///     A collection of general purpose utilities on dependency traversal.
/// </summary>
type Vagabond =

    /// <summary>
    ///     Initializes a new Vagabond instance.
    /// </summary>
    /// <param name="cacheDirectory">Temp folder used for assembly compilation and caching. Defaults to system temp folder.</param>
    /// <param name="profiles">Dynamic assembly configuration profiles.</param>
    /// <param name="typeConverter">FsPickler type name converter.</param>
    /// <param name="isIgnoredAssembly">User-defined assembly ignore predicate.</param>
    /// <param name="requireLoadedInAppDomain">
    ///     Demand all transitive dependencies be loadable in current AppDomain.
    ///     If unset, only loaded assemblies are listed as dependencies. Defaults to true.
    /// </param>
    /// <param name="loadPolicy">Default assembly load policy.</param>
    /// <param name="compressDataFiles">Compress data files generated by Vagabond. Defaults to true.</param>
    /// <param name="dataPersistThreshold">
    ///     Specifies persist threshold for data dependencies in bytes.
    ///     Objects exceeding the threshold will be persisted to files,
    ///     while all-others will be pickled in-memory. Defaults to 10 KiB.
    /// </param>
    static member Initialize(?cacheDirectory : string, ?profiles : IDynamicAssemblyProfile list, ?typeConverter : ITypeNameConverter, 
                                ?isIgnoredAssembly : Assembly -> bool, ?requireLoadedInAppDomain : bool, ?loadPolicy : AssemblyLoadPolicy, 
                                    ?compressDataFiles : bool, ?dataPersistTreshold : int64) : VagabondManager =
        new VagabondManager(?cacheDirectory = cacheDirectory, ?profiles = profiles, ?isIgnoredAssembly = isIgnoredAssembly, 
                        ?requireLoadedInAppDomain = requireLoadedInAppDomain, ?typeConverter = typeConverter, ?loadPolicy = loadPolicy, 
                        ?compressDataFiles = compressDataFiles, ?dataPersistTreshold = dataPersistTreshold)


    /// <summary>
    ///     Initializes a new Vagabond instance.
    /// </summary>
    /// <param name="ignoredAssemblies">Ignore assemblies and their dependencies.</param>
    /// <param name="cacheDirectory">Temp folder used for assembly compilation and caching. Defaults to system temp folder.</param>
    /// <param name="profiles">Dynamic assembly configuration profiles.</param>
    /// <param name="typeConverter">FsPickler type name converter.</param>
    /// <param name="requireLoadedInAppDomain">
    ///     Demand all transitive dependencies be loadable in current AppDomain.
    ///     If unset, only loaded assemblies are listed as dependencies. Defaults to true.
    /// </param>
    /// <param name="loadPolicy">Default assembly load policy.</param>
    /// <param name="compressDataFiles">Compress data files generated by Vagabond. Defaults to true.</param>
    /// <param name="dataPersistThreshold">
    ///     Specifies persist threshold for data dependencies in bytes.
    ///     Objects exceeding the threshold will be persisted to files,
    ///     while all-others will be pickled in-memory. Defaults to 10KiB.
    /// </param>
    static member Initialize(ignoredAssemblies : seq<Assembly>, ?cacheDirectory : string, ?profiles : IDynamicAssemblyProfile list,
                                ?typeConverter : ITypeNameConverter, ?requireLoadedInAppDomain : bool, ?loadPolicy : AssemblyLoadPolicy,
                                ?compressDataFiles : bool, ?dataPersistTreshold : int64) : VagabondManager =
        let traversedIgnored = traverseDependencies (fun _ -> false) false None ignoredAssemblies
        let ignoredSet = new System.Collections.Generic.HashSet<_>(traversedIgnored)
        new VagabondManager(?cacheDirectory = cacheDirectory, ?profiles = profiles, isIgnoredAssembly = ignoredSet.Contains, 
                        ?requireLoadedInAppDomain = requireLoadedInAppDomain, ?typeConverter = typeConverter, ?loadPolicy = loadPolicy, 
                        ?compressDataFiles = compressDataFiles, ?dataPersistTreshold = dataPersistTreshold)

    /// <summary>
    ///     Returns all type instances that appear in given object graph.
    /// </summary>
    /// <param name="obj">object graph to be traversed</param>
    static member ComputeTypeDependencies(obj:obj) : Type [] = gatherObjectDependencies obj |> fst

    /// <summary>
    ///     Resolves all assembly dependencies of given object graph.
    /// </summary>
    /// <param name="obj">object graph to be traversed</param>
    /// <param name="isIgnoredAssembly">User-defined assembly ignore predicate.</param>
    /// <param name="requireLoadedInAppDomain">
    ///     Demand all transitive dependencies be loadable in current AppDomain.
    ///     If unset, only loaded assemblies are listed as dependencies. Defaults to true.
    /// </param>
    static member ComputeAssemblyDependencies(obj:obj, ?isIgnoredAssembly, ?requireLoadedInAppDomain) : Assembly list =
        let requireLoadedInAppDomain = defaultArg requireLoadedInAppDomain true
        let isIgnoredAssembly = defaultArg isIgnoredAssembly (fun _ -> false)
        computeDependencies obj 
        |> Seq.map fst
        |> traverseDependencies isIgnoredAssembly requireLoadedInAppDomain None

    /// <summary>
    ///     Resolves all assembly dependencies of given assembly.
    /// </summary>
    /// <param name="assembly">assembly to be traversed</param>
    /// <param name="isIgnoredAssembly">User-defined assembly ignore predicate.</param>
    /// <param name="requireLoadedInAppDomain">
    ///     Demand all transitive dependencies be loadable in current AppDomain.
    ///     If unset, only loaded assemblies are listed as dependencies. Defaults to true.
    /// </param>
    static member ComputeAssemblyDependencies(assembly:Assembly, ?isIgnoredAssembly : Assembly -> bool, ?requireLoadedInAppDomain : bool) : Assembly list = 
        let requireLoadedInAppDomain = defaultArg requireLoadedInAppDomain true
        let isIgnoredAssembly = defaultArg isIgnoredAssembly (fun _ -> false)
        traverseDependencies isIgnoredAssembly requireLoadedInAppDomain None [assembly]

    /// <summary>
    ///     Resolves all assembly dependencies of given assemblies.
    /// </summary>
    /// <param name="assemblies">Starting assembly dependencies.</param>
    /// <param name="isIgnoredAssembly">User-defined assembly ignore predicate.</param>
    /// <param name="requireLoadedInAppDomain">
    ///     Demand all transitive dependencies be loadable in current AppDomain.
    ///     If unset, only loaded assemblies are listed as dependencies. Defaults to true.
    /// </param>
    static member ComputeAssemblyDependencies(assemblies:seq<Assembly>, ?isIgnoredAssembly : Assembly -> bool, ?requireLoadedInAppDomain : bool) : Assembly list = 
        let requireLoadedInAppDomain = defaultArg requireLoadedInAppDomain true
        let isIgnoredAssembly = defaultArg isIgnoredAssembly (fun _ -> false)
        traverseDependencies isIgnoredAssembly requireLoadedInAppDomain None assemblies


    /// <summary>
    ///     Computes a unique id for given static assembly.
    /// </summary>
    /// <param name="assembly">Input static assembly.</param>
    static member ComputeAssemblyId (assembly : Assembly) : AssemblyId = 
        let _ = assembly.Location // force exception in case of dynamic assembly
        assembly.AssemblyId

    /// <summary>
    ///     Computes unique id's for a collection of static assemblies.
    /// </summary>
    /// <param name="assemblies">Input static assemblies</param>
    static member ComputeAssemblyIds (assemblies : seq<Assembly>) : AssemblyId list =
        assemblies |> Seq.map Vagabond.ComputeAssemblyId |> Seq.toList

    /// <summary>
    ///     Generates a unique file name that corresponds to the particular assembly id.
    ///     Useful for caching implementations.
    /// </summary>
    /// <param name="id">Input assembly identifier</param>
    static member GetFileName(id : AssemblyId) : string = id.GetFileName()