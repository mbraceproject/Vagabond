namespace Nessos.Vagrant
    
    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.DependencyAnalysis
    open Nessos.Vagrant.Daemon

    type Vagrant (?cacheDirectory : string, ?profiles : IDynamicAssemblyProfile list, ?typeConverter : ITypeNameConverter, ?loadPolicy) =

        let cacheDirectory = match cacheDirectory with None -> Path.GetTempPath() | Some d -> d

        let profiles = 
            match profiles with
            | Some ps -> ps
            | None -> [ new FsiDynamicAssemblyProfile() :> IDynamicAssemblyProfile ]

        let mutable _loadPolicy = defaultArg loadPolicy <| AssemblyLoadPolicy.ResolveStrongNames

        let daemon = new VagrantDaemon(cacheDirectory, profiles, ?tyConv = typeConverter)

        do daemon.Start()

        static let checkIsDynamic(a : Assembly) = 
            if a.IsDynamic then () 
            else invalidArg a.FullName "Vagrant: not a dynamic assembly"

        let compile (assemblies : Assembly list) = 
            daemon.PostAndReply(fun ch -> CompileDynamicAssemblySlice(assemblies, ch))

        /// Unique identifier for the slice compiler
        member __.UUId = daemon.CompilerState.ServerId
        /// Returns the pickler used by the slice compiler
        member __.Pickler = daemon.DefaultPickler
        /// FsPickler type name converter for use with other formats
        member __.TypeConverter = daemon.TypeNameConverter

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

            remapDependencies daemon.CompilerState dependencies

        member __.IsLocalDynamicAssemblySlice(id : AssemblyId) =
            isLocalDynamicAssemblySlice daemon.CompilerState id

        /// <summary>
        ///     Returns the dynamic assembly slice corresponding to the given type, if exists.
        /// </summary>
        /// <param name="t"></param>
        member __.TryGetSliceOfType(t : Type) =
            let t = if t.IsGenericType && not t.IsGenericTypeDefinition then t.GetGenericTypeDefinition() else t
            match daemon.CompilerState.DynamicAssemblies.TryFind t.Assembly.FullName with
            | None -> None
            | Some dyn -> dyn.TryGetSlice t |> Option.map (fun s -> s.Assembly)


        member __.CreatePortableAssembly(id : AssemblyId, includeAssemblyImage : bool, ?loadPolicy) =
            let loadPolicy = defaultArg loadPolicy _loadPolicy
            daemon.PostAndReply(fun ch -> GetPortableAssembly(loadPolicy, includeAssemblyImage, id, ch))

        member __.CreatePortableAssemblies(ids : seq<AssemblyId>, includeAssemblyImage : bool, ?loadPolicy) =
            Seq.toList ids
            |> List.map (fun id -> __.CreatePortableAssembly(id, includeAssemblyImage, ?loadPolicy = loadPolicy))
            

        /// <summary>
        ///     Builds a portable assembly bundle for given input.
        /// </summary>
        /// <param name="assembly">Given assembly.</param>
        /// <param name="includeAssemblyImage">Include raw assembly image in the bundle.</param>
        member __.CreatePortableAssembly(assembly : Assembly, includeAssemblyImage:bool) =
            __.CreatePortableAssembly(assembly.AssemblyId, includeAssemblyImage)


        member __.GetAssemblyLoadInfo(id : AssemblyId, ?loadPolicy) =
            let loadPolicy = defaultArg loadPolicy _loadPolicy
            daemon.PostAndReply(fun ch -> GetAssemblyLoadInfo(loadPolicy, id, ch))

        member __.GetAssemblyLoadInfo(ids : seq<AssemblyId>, ?loadPolicy) =
            Seq.toList ids
            |> List.map (fun id -> __.GetAssemblyLoadInfo(id, ?loadPolicy = loadPolicy))


        member __.LoadPortableAssembly(pa : PortableAssembly, ?loadPolicy) =
            let loadPolicy = defaultArg loadPolicy _loadPolicy
            daemon.PostAndReply(fun ch -> LoadAssembly(loadPolicy, pa, ch))

        member __.LoadPortableAssemblies(pas : seq<PortableAssembly>, ?loadPolicy) =
            Seq.toList pas
            |> List.map (fun pa -> __.LoadPortableAssembly(pa, ?loadPolicy = loadPolicy))