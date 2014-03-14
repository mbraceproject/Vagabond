namespace Nessos.Vagrant
    
    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.Serialization
    open Nessos.Vagrant.DependencyAnalysis
    open Nessos.Vagrant.BlobManagement
    open Nessos.Vagrant.SliceCompiler

    /// <summary>
    ///     A collection of general purpose utilities used by Vagrant
    /// </summary>
    type Utilities =

        /// <summary>
        ///     Returns all type instances that appear in given object graph.
        /// </summary>
        /// <param name="obj">object graph to be traversed</param>
        static member ComputeTypeDependencies(obj:obj) : Type [] = gatherObjectDependencies obj

        /// <summary>
        ///     Resolves all assembly dependencies of given object graph.
        /// </summary>
        /// <param name="obj">object graph to be traversed</param>
        static member ComputeAssemblyDependencies(obj:obj) =
            computeDependencies obj 
            |> Seq.map fst
            |> traverseDependencies None

        /// <summary>
        ///     Resolves all assembly dependencies of given assembly.
        /// </summary>
        /// <param name="assembly">assembly to be traversed</param>
        static member ComputeAssemblyDependencies(assembly:Assembly) = 
            traverseDependencies None [assembly]

        /// <summary>
        ///     Resolves all assembly dependencies of given assemblies.
        /// </summary>
        /// <param name="assemblies"></param>
        static member ComputeAssemblyDependencies(assemblies:seq<Assembly>) = 
            traverseDependencies None assemblies



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
        member __.LoadTypeInitializers(info : DynamicAssemblySliceInfo) = loader.Invoke info

        /// <summary>
        ///     Loads the type initializers from given dependency packages.
        /// </summary>
        /// <param name="info">Dependency info packages to be loaded.</param>
        member __.LoadTypeInitializers(info : seq<DynamicAssemblySliceInfo>) =
            info |> Seq.collect loader.Invoke |> Seq.toList

        /// <summary>
        ///     Get latest generation of initialization blobs for given assembly
        /// </summary>
        /// <param name="assembly">given loaded assembly</param>
        member __.TryGetLoadedGenerationForAssembly(assembly : Assembly) =
            loader.CurrentState.TryFind assembly.FullName
            


    /// <summary>
    ///     Compilation server for dynamic assemblies.
    /// </summary>
    /// <param name="outpath">specifies a target directory for the compiler.</param>
    /// <param name="picklerRegistry">specifies a custom pickler registry.</param>
    [<Sealed>]
    [<AutoSerializable(false)>]
    type VagrantServer(?outpath : string, ?dynamicAssemblyProfiles : IDynamicAssemblyProfile list ,?picklerRegistry : CustomPicklerRegistry) =

        let outpath = 
            match outpath with
            | Some path when Directory.Exists path -> path
            | Some _ -> invalidArg "outPath" "not a valid directory."
            | None -> Path.GetTempPath()

        let dynamicAssemblyProfiles = 
            match dynamicAssemblyProfiles with
            | Some ps -> ps
            | None -> [ new FsiDynamicAssemblyProfile() :> IDynamicAssemblyProfile ]

        do Singleton.acquire "VagrantServer"

        // initialize agents

        let compiler = mkCompilationAgent dynamicAssemblyProfiles outpath
        let pickler = mkFsPicklerInstance picklerRegistry (fun () -> compiler.CurrentState)
        let exporter = mkExporterAgent pickler (fun () -> compiler.CurrentState)
        let client = new VagrantClient(pickler, Some compiler.CurrentState.ServerId)

        let compile (assemblies : Assembly list) = compiler.Invoke(assemblies).Value

        /// Unique identifier for the slice compiler
        member __.UUId = compiler.CurrentState.ServerId
        /// Returns the pickler used by the slice compiler
        member __.Pickler = pickler
        /// Returns the included 
        member __.Client = client

        /// <summary>
        ///     Compiles slices for given dynamic assembly, if required.
        /// </summary>
        /// <param name="assembly">a dynamic assembly</param>
        member __.CompileDynamicAssemblySlice (assembly : Assembly) =
            if assembly.IsDynamic then 
                compile [assembly] |> List.map (fun slice -> exporter.Invoke (true, slice.Assembly))

            else invalidArg assembly.FullName "Vagrant: not a dynamic assembly."

        /// <summary>
        ///     Compiles slices for given dynamic assembly, if required.
        /// </summary>
        /// <param name="assembly">a dynamic assembly</param>
        /// <param name="generateTypeInitializer">generate a type initialization blob for the assembly. Defaults to true.</param>
        member __.CompileDynamicAssemblySlices (assemblies : seq<Assembly>, ?generateTypeInitializer : bool) =
            compile (Seq.toList assemblies) |> List.map (fun slice -> exporter.Invoke (true, slice.Assembly))

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
                |> Map.toList
                |> List.map (fun (_,(_,s)) -> s)

        /// <summary>
        ///     Creates an exportable DependencyInfo bundle for given assembly.
        ///     This includes potential type initialization data for dynamic assembly slices.
        /// </summary>
        /// <param name="assembly">a static assembly</param>
        /// <param name="getTypeInitializationBlobs">include dump of designated static field pickles. Defaults to true.</param>
        member __.GetDependencyInfo(assembly : Assembly, ?getTypeInitializationBlobs) = 
            let getTypeInitializationBlobs = defaultArg getTypeInitializationBlobs true
            exporter.Invoke (getTypeInitializationBlobs, assembly)

        /// <summary>
        ///     Creates exportable DependencyInfo bundles for given assemblies.
        ///     This includes potential type initialization data for dynamic assembly slices.
        /// </summary>
        /// <param name="assemblies">a collection of static assemblies.</param>
        /// <param name="getTypeInitializationBlobs">include dump of designated static field pickles. Defaults to true.</param>
        member c.GetDependencyInfo(assemblies : seq<Assembly>, ?getTypeInitializationBlobs) =
            assemblies 
            |> Seq.map (fun a -> c.GetDependencyInfo(a, ?getTypeInitializationBlobs = getTypeInitializationBlobs)) 
            |> Seq.toList


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