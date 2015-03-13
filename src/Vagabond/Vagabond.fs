namespace Nessos.Vagabond
    
open System
open System.IO
open System.Reflection

open Nessos.FsPickler

open Nessos.Vagabond.Utils
open Nessos.Vagabond.DependencyAnalysis
open Nessos.Vagabond.Control

/// Vagabond Object which instantiates a dynamic assembly compiler, loader and exporter state

[<AutoSerializable(false)>]
type Vagabond private (?cacheDirectory : string, ?profiles : IDynamicAssemblyProfile list, ?typeConverter : ITypeNameConverter, 
                        ?isIgnoredAssembly : Assembly -> bool, ?requireLoadedInAppDomain, ?loadPolicy : AssemblyLoadPolicy,
                        ?compressStaticData : bool) =

    static do AssemblyManagement.registerAssemblyResolutionHandler ()

    let cacheDirectory = 
        match cacheDirectory with 
        | Some d when Directory.Exists d -> d
        | Some d -> raise <| new DirectoryNotFoundException(d)
        | None -> Path.GetTempPath()

    let isIgnoredAssembly = defaultArg isIgnoredAssembly (fun _ -> false)
    let requireLoadedInAppDomain = defaultArg requireLoadedInAppDomain true
    let compressStaticData = defaultArg compressStaticData true

    let profiles =
        match profiles with
        | Some ps -> ps
        | None -> [ new FsiDynamicAssemblyProfile() :> IDynamicAssemblyProfile ]

    let mutable _loadPolicy = defaultArg loadPolicy <| AssemblyLoadPolicy.ResolveStrongNames

    let controller = new VagabondController(cacheDirectory, profiles, requireLoadedInAppDomain, compressStaticData, isIgnoredAssembly, ?tyConv = typeConverter)
    do controller.Start()

    static let checkIsDynamic(a : Assembly) = 
        if a.IsDynamic then () 
        else invalidArg a.FullName "Vagabond: not a dynamic assembly"

    let compile (assemblies : Assembly list) = 
        controller.PostAndReply(fun ch -> CompileDynamicAssemblySlice(assemblies, ch))

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
    /// <param name="compressStaticData">Compress static data initializers for dynamic assemblies. Defaults to true.</param>
    static member Initialize(?cacheDirectory : string, ?profiles : IDynamicAssemblyProfile list, ?typeConverter : ITypeNameConverter, 
                                ?isIgnoredAssembly : Assembly -> bool, ?requireLoadedInAppDomain : bool, ?loadPolicy : AssemblyLoadPolicy, ?compressStaticData : bool) =
        new Vagabond(?cacheDirectory = cacheDirectory, ?profiles = profiles, ?isIgnoredAssembly = isIgnoredAssembly, 
                        ?requireLoadedInAppDomain = requireLoadedInAppDomain, ?typeConverter = typeConverter, ?loadPolicy = loadPolicy, ?compressStaticData = compressStaticData)


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
    static member Initialize(ignoredAssemblies : seq<Assembly>, ?cacheDirectory : string, ?profiles : IDynamicAssemblyProfile list,
                                ?typeConverter : ITypeNameConverter, ?requireLoadedInAppDomain : bool, ?loadPolicy : AssemblyLoadPolicy) =
        let traversedIgnored = traverseDependencies (fun _ -> false) false None ignoredAssemblies
        let ignoredSet = new System.Collections.Generic.HashSet<_>(traversedIgnored)
        new Vagabond(?cacheDirectory = cacheDirectory, ?profiles = profiles, isIgnoredAssembly = ignoredSet.Contains, 
                        ?requireLoadedInAppDomain = requireLoadedInAppDomain, ?typeConverter = typeConverter, ?loadPolicy = loadPolicy)

    /// Unique identifier for the slice compiler
    member __.UUId = controller.CompilerState.CompilerId
    /// Returns the pickler used by the slice compiler
    member __.Pickler = controller.DefaultPickler
    /// FsPickler type name converter for use with other formats
    member __.TypeConverter = controller.TypeNameConverter
    /// Cache directory used by Vagabond
    member __.CachePath = controller.CacheDirectory

    /// Default load policy 
    member __.DefaultLoadPolicy
        with get () = _loadPolicy
        and set p = _loadPolicy <- p

    /// <summary>
    ///     Compiles slices for given dynamic assembly, if required.
    /// </summary>
    /// <param name="assembly">a dynamic assembly</param>
    member __.CompileDynamicAssemblySlice (assembly : Assembly) =
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
    member __.GetDynamicAssemblySlices(assembly : Assembly) =
        do checkIsDynamic assembly
        match controller.CompilerState.DynamicAssemblies.TryFind assembly.FullName with
        | None -> []
        | Some info ->
            info.GeneratedSlices |> Seq.map (function KeyValue(_,s) -> s.Assembly) |> Seq.toList

    /// <summary>
    ///     Returns a collection of all assemblies that the given object depends on.
    ///     Dynamic assemblies are substituted for their corresponding static slices.
    /// </summary>
    /// <param name="obj">A given object graph</param>
    /// <param name="permitCompilation">Compile new slices as required. Defaults to false.</param>
    member __.ComputeObjectDependencies(obj : obj, ?permitCompilation : bool) =
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
    member __.IsLocalDynamicAssemblySlice(id : AssemblyId) =
        controller.CompilerState.IsLocalDynamicAssemblySlice id

    /// <summary>
    ///     Returns the dynamic assembly slice corresponding to the given type, if exists.
    /// </summary>
    /// <param name="t">input type.</param>
    member __.TryGetSliceOfType(t : Type) =
        let t = if t.IsGenericType && not t.IsGenericTypeDefinition then t.GetGenericTypeDefinition() else t
        match controller.CompilerState.DynamicAssemblies.TryFind t.Assembly.FullName with
        | None -> None
        | Some dyn -> dyn.TryGetSlice t |> Option.map (fun s -> s.Assembly)

        
    /// <summary>
    ///     Creates a vagabond assembly package out of a given assembly id.
    /// </summary>
    /// <param name="id">assembly id</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to strong names only.</param>
    member __.CreateVagabondAssembly(id : AssemblyId, ?loadPolicy : AssemblyLoadPolicy) =
        let loadPolicy = defaultArg loadPolicy _loadPolicy
        controller.PostAndReply(fun ch -> GetVagabondAssembly(loadPolicy, id, ch))

    /// <summary>
    ///     Creates assembly packages out of given assembly ids.
    /// </summary>
    /// <param name="ids"></param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.CreateVagabondAssemblies(ids : seq<AssemblyId>, ?loadPolicy : AssemblyLoadPolicy) =
        Seq.toList ids
        |> List.map (fun id -> __.CreateVagabondAssembly(id, ?loadPolicy = loadPolicy))
            

    /// <summary>
    ///     Builds an assembly package bundle for given input.
    /// </summary>
    /// <param name="assembly">Given assembly.</param>
    member __.CreateVagabondAssembly(assembly : Assembly) =
        __.CreateVagabondAssembly(assembly.AssemblyId)

    /// <summary>
    ///     Creates assembly packages out of given assemblies.
    /// </summary>
    /// <param name="assemblies">Inputs assemblies.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.CreateVagabondAssemblies(assemblies : seq<Assembly>, ?loadPolicy : AssemblyLoadPolicy) =
        Seq.toList assemblies
        |> List.map (fun asm -> __.CreateVagabondAssembly(asm.AssemblyId, ?loadPolicy = loadPolicy))


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
    member __.GetAssemblyLoadInfo(ids : seq<AssemblyId>, ?loadPolicy : AssemblyLoadPolicy) =
        Seq.toList ids
        |> List.map (fun id -> __.GetAssemblyLoadInfo(id, ?loadPolicy = loadPolicy))

    /// <summary>
    ///     Loads assembly package to the local machine.
    /// </summary>
    /// <param name="va">Input assembly package.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.LoadVagabondAssembly(va : VagabondAssembly, ?loadPolicy : AssemblyLoadPolicy) =
        let loadPolicy = defaultArg loadPolicy _loadPolicy
        controller.PostAndReply(fun ch -> LoadAssembly(loadPolicy, va, ch))

    /// <summary>
    ///     Loads assembly packages to the local machine.
    /// </summary>
    /// <param name="vas">Input assembly packages.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.LoadVagabondAssemblies(vas : seq<VagabondAssembly>, ?loadPolicy : AssemblyLoadPolicy) =
        Seq.toList vas
        |> List.map (fun va -> __.LoadVagabondAssembly(va, ?loadPolicy = loadPolicy))

    /// <summary>
    ///     Loads an assembly that is already cached in local machine.
    /// </summary>
    /// <param name="id">input assembly id.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.LoadVagabondAssembly(id : AssemblyId, ?loadPolicy : AssemblyLoadPolicy) =
        match controller.AssemblyCache.TryGetCachedAssemblyInfo id with
        | None -> __.GetAssemblyLoadInfo(id, ?loadPolicy = loadPolicy)
        | Some va -> __.LoadVagabondAssembly(va, ?loadPolicy = loadPolicy)

    /// <summary>
    ///     Loads assembly id's that are already cached in local machine.
    /// </summary>
    /// <param name="id">input assembly id.</param>
    /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
    member __.LoadVagabondAssemblies(ids : seq<AssemblyId>, ?loadPolicy : AssemblyLoadPolicy) =
        Seq.toList ids
        |> List.map (fun id -> __.LoadVagabondAssembly(id, ?loadPolicy = loadPolicy))


    /// <summary>
    ///     Export provided assemblies using provided exporter
    /// </summary>
    /// <param name="exporter">Assembly exporter implementation.</param>
    /// <param name="assemblies">Assemblies to be exported.</param>
    member __.ExportAssemblies(exporter : IAssemblyExporter, assemblies : seq<VagabondAssembly>) = async {
        return! controller.PostAndAsyncReply(fun ch -> ExportAssemblies(exporter, Seq.toList assemblies, ch))
    }

    /// <summary>
    ///     Import provided assemblies to cache using provided importer.
    /// </summary>
    /// <param name="importer">Assembly importer implementation.</param>
    /// <param name="ids">Assembly id's to be imported.</param>
    member __.ImportAssemblies(importer : IAssemblyImporter, ids : seq<AssemblyId>) = async {
        return! controller.PostAndAsyncReply(fun ch -> ImportAssemblies(importer, Seq.toList ids, ch))
    }