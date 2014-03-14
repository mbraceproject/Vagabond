namespace Nessos.Vagrant
    
    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.Serialization
    open Nessos.Vagrant.DependencyAnalysis
    open Nessos.Vagrant.DependencyExporter
    open Nessos.Vagrant.SliceCompiler

    module private Singleton =
        let singleton = new Singleton<string>()

        let acquire (id : string) =
            if singleton.TryAcquire id then ()
            else
                invalidOp <| sprintf "Vagrant: an instance of '%s' has already been initialized." singleton.Content.Value

    /// <summary>
    ///     Client for loading type initialization blobs of dynamic assembly slices.
    /// </summary>
    [<Sealed>]
    [<AutoSerializable(false)>]
    type VagrantClient internal (pickler : FsPickler, localServerId : Guid option) =

        let loader = mkLoaderAgent pickler localServerId

        /// <summary>
        ///     Client for loading type initialization blobs of dynamic assembly slices.
        /// </summary>
        /// <param name="pickler">Specify a custom pickler instance.</param>
        new (?pickler : FsPickler) =
            Singleton.acquire "VagrantClient"
            let pickler = match pickler with None -> new FsPickler() | Some p -> p
            new VagrantClient(pickler, None)

        /// Returns pickler used in type initialization.
        member __.Pickler = pickler

        /// <summary>
        ///     Loads the type initializers from given dependency package.
        /// </summary>
        /// <param name="info">Dependency info package to be loaded.</param>
        member __.LoadTypeInitializers(info : DependencyInfo) = loader.Invoke info

        /// <summary>
        ///     Loads the type initializers from given dependency packages.
        /// </summary>
        /// <param name="info">Dependency info packages to be loaded.</param>
        member __.LoadTypeInitializers(info : seq<DependencyInfo>) =
            for i in info do loader.Invoke i


    /// <summary>
    ///     Compilation server for dynamic assemblies.
    /// </summary>
    /// <param name="outpath">specifies a target directory for the compiler.</param>
    /// <param name="picklerRegistry">specifies a custom pickler registry.</param>
    [<Sealed>]
    [<AutoSerializable(false)>]
    type VagrantServer(?outpath : string, ?picklerRegistry : CustomPicklerRegistry) =

        let outpath = 
            match outpath with
            | Some path when Directory.Exists path -> path
            | Some _ -> invalidArg "outPath" "not a valid directory."
            | None -> Path.GetTempPath()

        do Singleton.acquire "VagrantServer"

        // initialize agents

        let compiler = mkCompilationAgent outpath
        let pickler = mkFsPicklerInstance picklerRegistry (fun () -> compiler.CurrentState)
        let exporter = mkExporterAgent pickler (fun () -> compiler.CurrentState)
        let client = new VagrantClient(pickler, Some compiler.CurrentState.ServerId)

        let compile (assemblies : Assembly list) =
            match compiler.Invoke assemblies with
            | Choice1Of2 slices -> slices
            | Choice2Of2 e -> raise e

        /// Unique identifier for the slice compiler
        member __.UUId = compiler.CurrentState.ServerId
        /// Returns the pickler used by the slice compiler
        member __.Pickler = pickler
        /// Returns the included 
        member __.Client = client

        /// <summary>
        ///     Compiles a slice for given dynamic assembly, if required.
        /// </summary>
        /// <param name="assembly">a dynamic assembly</param>
        member __.CompileDynamicAssemblySlice (assembly : Assembly) =
            if assembly.IsDynamic then
                let newSlices = compile [assembly]
                newSlices |> List.map (fun slice -> exporter.Invoke slice.Assembly)

            else invalidArg assembly.FullName "Vagrant: not a dynamic assembly."

        /// <summary>
        ///     Returns a list of dynamic assemblies that require slice compilation
        ///     for the given object graph to be exportable.
        /// </summary>
        /// <param name="obj">any object graph</param>
        member __.ResolveDynamicDependenciesRequiringCompilation(obj : obj) : Assembly list =
            let dependencies = computeDependencies obj
            getDynamicDependenciesRequiringCompilation compiler.CurrentState dependencies

        /// <summary>
        ///     Returns *all* assembly slices of given dynamic assembly.
        /// </summary>
        /// <param name="assembly">a dynamic assembly.</param>
        member __.GetDynamicAssemblySlices(assembly : Assembly) =
            match compiler.CurrentState.DynamicAssemblies.TryFind assembly.FullName with
            | None -> []
            | Some info ->
                info.GeneratedSlices
                |> Map.toSeq
                |> Seq.sortBy (fun (_,s) -> s.SliceId) 
                |> Seq.map (fun (_,s) -> s.Assembly)
                |> Seq.toList

        /// <summary>
        ///     Creates an exportable DependencyInfo bundle for given assembly.
        ///     This includes potential type initialization data for dynamic assembly slices.
        /// </summary>
        /// <param name="assembly">a static assembly</param>
        member __.GetDependencyInfo(assembly : Assembly) = exporter.Invoke assembly

        /// <summary>
        ///     Creates exportable DependencyInfo bundles for given assemblies.
        ///     This includes potential type initialization data for dynamic assembly slices.
        /// </summary>
        /// <param name="assemblies">a collection of static assemblies.</param>
        member __.GetDependencyInfo(assemblies : seq<Assembly>) = Seq.map exporter.Invoke assemblies |> Seq.toList


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
                let assemblies = getDynamicDependenciesRequiringCompilation compiler.CurrentState dependencies
                let _ = compile assemblies in ()

            remapDependencies compiler.CurrentState dependencies

        /// <summary>
        ///     Returns an exportable pickle and a collection of all assemblies that the given object depends on.
        ///     Dynamic assemblies are substituted for their corresponding static slices.
        /// </summary>
        /// <param name="obj">A given object graph</param>
        /// <param name="permitCompilation">Compile new slices as required. Defaults to false.</param>
        member __.ComputePickle(obj, ?permitCompilation : bool) : byte [] * Assembly list =
            let dependencies = __.ComputeObjectDependencies(obj, ?permitCompilation = permitCompilation)
            let pickle = pickler.Pickle obj
            pickle, dependencies