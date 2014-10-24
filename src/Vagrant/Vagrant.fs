namespace Nessos.Vagrant
    
    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.DependencyAnalysis
    open Nessos.Vagrant.Daemon

    /// Vagrant Object which instantiates a dynamic assembly compiler, loader and exporter state

    [<AutoSerializable(false)>]
    type Vagrant private (?cacheDirectory : string, ?profiles : IDynamicAssemblyProfile list, ?typeConverter : ITypeNameConverter, 
                            ?isIgnoredAssembly : Assembly -> bool, ?requireLoadedInAppDomain, ?loadPolicy : AssemblyLoadPolicy) =

        static do AssemblyManagement.registerAssemblyResolutionHandler ()

        let cacheDirectory = 
            match cacheDirectory with 
            | Some d when Directory.Exists d -> d
            | Some d -> raise <| new DirectoryNotFoundException(d)
            | None -> Path.GetTempPath()

        let isIgnoredAssembly = defaultArg isIgnoredAssembly (fun _ -> false)
        let requireLoadedInAppDomain = defaultArg requireLoadedInAppDomain true

        let profiles =
            match profiles with
            | Some ps -> ps
            | None -> [ new FsiDynamicAssemblyProfile() :> IDynamicAssemblyProfile ]

        let mutable _loadPolicy = defaultArg loadPolicy <| AssemblyLoadPolicy.ResolveStrongNames

        let daemon = new VagrantDaemon(cacheDirectory, profiles, requireLoadedInAppDomain, isIgnoredAssembly, ?tyConv = typeConverter)

        do daemon.Start()

        static let checkIsDynamic(a : Assembly) = 
            if a.IsDynamic then () 
            else invalidArg a.FullName "Vagrant: not a dynamic assembly"

        let compile (assemblies : Assembly list) = 
            daemon.PostAndReply(fun ch -> CompileDynamicAssemblySlice(assemblies, ch))

        /// <summary>
        ///     Initializes a new Vagrant instance.
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
        static member Initialize(?cacheDirectory : string, ?profiles : IDynamicAssemblyProfile list, ?typeConverter : ITypeNameConverter, 
                                    ?isIgnoredAssembly : Assembly -> bool, ?requireLoadedInAppDomain : bool, ?loadPolicy : AssemblyLoadPolicy) =
            new Vagrant(?cacheDirectory = cacheDirectory, ?profiles = profiles, ?isIgnoredAssembly = isIgnoredAssembly, 
                            ?requireLoadedInAppDomain = requireLoadedInAppDomain, ?typeConverter = typeConverter, ?loadPolicy = loadPolicy)

        /// Unique identifier for the slice compiler
        member __.UUId = daemon.CompilerState.ServerId
        /// Returns the pickler used by the slice compiler
        member __.Pickler = daemon.DefaultPickler
        /// FsPickler type name converter for use with other formats
        member __.TypeConverter = daemon.TypeNameConverter
        /// Cache directory used by Vagrant
        member __.CachePath = daemon.CacheDirectory

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
            getDynamicDependenciesRequiringCompilation daemon.CompilerState dependencies

        /// <summary>
        ///     Returns *all* assembly slices of given dynamic assembly.
        /// </summary>
        /// <param name="assembly">a dynamic assembly.</param>
        member __.GetDynamicAssemblySlices(assembly : Assembly) =
            do checkIsDynamic assembly
            match daemon.CompilerState.DynamicAssemblies.TryFind assembly.FullName with
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
                let assemblies = getDynamicDependenciesRequiringCompilation daemon.CompilerState dependencies
                let _ = compile assemblies in ()

            remapDependencies isIgnoredAssembly requireLoadedInAppDomain daemon.CompilerState dependencies

        /// <summary>
        ///     Checks if assembly id is a locally generated dynamic assembly slice.
        /// </summary>
        /// <param name="id">input assembly id.</param>
        member __.IsLocalDynamicAssemblySlice(id : AssemblyId) =
            daemon.CompilerState.IsLocalDynamicAssemblySlice id

        /// <summary>
        ///     Returns the dynamic assembly slice corresponding to the given type, if exists.
        /// </summary>
        /// <param name="t">input type.</param>
        member __.TryGetSliceOfType(t : Type) =
            let t = if t.IsGenericType && not t.IsGenericTypeDefinition then t.GetGenericTypeDefinition() else t
            match daemon.CompilerState.DynamicAssemblies.TryFind t.Assembly.FullName with
            | None -> None
            | Some dyn -> dyn.TryGetSlice t |> Option.map (fun s -> s.Assembly)

        
        /// <summary>
        ///     Creates an assembly package out of a given assembly id.
        /// </summary>
        /// <param name="id">assembly id</param>
        /// <param name="includeAssemblyImage">include assembly image in assembly package.</param>
        /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to strong names only.</param>
        member __.CreateAssemblyPackage(id : AssemblyId, includeAssemblyImage : bool, ?loadPolicy) =
            let loadPolicy = defaultArg loadPolicy _loadPolicy
            daemon.PostAndReply(fun ch -> GetAssemblyPackage(loadPolicy, includeAssemblyImage, id, ch))

        /// <summary>
        ///     Creates assembly packages out of given assembly ids.
        /// </summary>
        /// <param name="ids"></param>
        /// <param name="includeAssemblyImage">Include raw assembly image in the bundle.</param>
        /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
        member __.CreateAssemblyPackages(ids : seq<AssemblyId>, includeAssemblyImage : bool, ?loadPolicy) =
            Seq.toList ids
            |> List.map (fun id -> __.CreateAssemblyPackage(id, includeAssemblyImage, ?loadPolicy = loadPolicy))
            

        /// <summary>
        ///     Builds an assembly package bundle for given input.
        /// </summary>
        /// <param name="assembly">Given assembly.</param>
        /// <param name="includeAssemblyImage">Include raw assembly image in the bundle.</param>
        member __.CreateAssemblyPackage(assembly : Assembly, includeAssemblyImage:bool) =
            __.CreateAssemblyPackage(assembly.AssemblyId, includeAssemblyImage)


        /// <summary>
        ///     Gets the local assembly load info for given assembly id.
        /// </summary>
        /// <param name="id">Given assembly id.</param>
        /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
        member __.GetAssemblyLoadInfo(id : AssemblyId, ?loadPolicy) =
            let loadPolicy = defaultArg loadPolicy _loadPolicy
            daemon.PostAndReply(fun ch -> GetAssemblyLoadInfo(loadPolicy, id, ch))

        /// <summary>
        ///     Gets the local assembly load info for given assembly ids.
        /// </summary>
        /// <param name="ids">Given assembly ids.</param>
        /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
        member __.GetAssemblyLoadInfo(ids : seq<AssemblyId>, ?loadPolicy) =
            Seq.toList ids
            |> List.map (fun id -> __.GetAssemblyLoadInfo(id, ?loadPolicy = loadPolicy))

        /// <summary>
        ///     Loads assembly package to the local machine.
        /// </summary>
        /// <param name="pa">Input assembly package.</param>
        /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
        member __.LoadAssemblyPackage(pa : AssemblyPackage, ?loadPolicy) =
            let loadPolicy = defaultArg loadPolicy _loadPolicy
            daemon.PostAndReply(fun ch -> LoadAssembly(loadPolicy, pa, ch))

        /// <summary>
        ///     Loads assembly packages to the local machine.
        /// </summary>
        /// <param name="pas">Input assembly packages.</param>
        /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
        member __.LoadAssemblyPackages(pas : seq<AssemblyPackage>, ?loadPolicy) =
            Seq.toList pas
            |> List.map (fun pa -> __.LoadAssemblyPackage(pa, ?loadPolicy = loadPolicy))

        /// <summary>
        ///     Loads an assembly that is already cached in local machine.
        /// </summary>
        /// <param name="id">input assembly id.</param>
        /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
        member __.LoadCachedAssembly(id : AssemblyId, ?loadPolicy) =
            __.LoadAssemblyPackage(AssemblyPackage.Empty id, ?loadPolicy = loadPolicy)

        /// <summary>
        ///     Loads assembly id's that are already cached in local machine.
        /// </summary>
        /// <param name="id">input assembly id.</param>
        /// <param name="loadPolicy">Specifies assembly resolution policy. Defaults to resolving strong names only.</param>
        member __.LoadCachedAssemblies(ids : seq<AssemblyId>, ?loadPolicy) =
            Seq.toList ids
            |> List.map (fun id -> __.LoadAssemblyPackage(AssemblyPackage.Empty id, ?loadPolicy = loadPolicy))