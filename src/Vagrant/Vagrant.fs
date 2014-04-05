namespace Nessos.Vagrant
    
    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.Serialization
    open Nessos.Vagrant.DependencyAnalysis
    open Nessos.Vagrant.AssemblyExporter
    open Nessos.Vagrant.SliceCompilerImpls

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


        static member TryGetFsiDynamicAssembly() =
            System.AppDomain.CurrentDomain.GetAssemblies() 
            |> Array.tryFind(fun a -> a.IsDynamic && a.GetName().Name = "FSI-ASSEMBLY")


        static member TryGetFsiInteractionTypes () =
            match Utilities.TryGetFsiDynamicAssembly() with
            | None -> None
            | Some a ->
                let fsiRegex = new System.Text.RegularExpressions.Regex("^FSI_[0-9]{4}$")
                a.GetTypes() |> Array.filter (fun t -> fsiRegex.IsMatch t.Name) |> Some

        static member TryGetLatestFsiInteraction () =
            Utilities.TryGetFsiInteractionTypes() 
            |> Option.map (fun ts -> ts |> Seq.sortBy (fun t -> t.Name) |> Seq.last)




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

        let loader = mkAssemblyLoader pickler localServerId

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
        member __.LoadPortableAssembly(assembly : PortableAssembly) = loader.PostAndReply assembly

        /// <summary>
        ///     Loads the type initializers from given dependency packages.
        /// </summary>
        /// <param name="info">Dependency info packages to be loaded.</param>
        member __.LoadPortableAssemblies(assemblies : seq<PortableAssembly>) =
            assemblies |> Seq.map loader.PostAndReply |> Seq.toList
            


    /// <summary>
    ///     Compilation server for dynamic assemblies.
    /// </summary>
    /// <param name="outpath">specifies a target directory for the compiler.</param>
    /// <param name="picklerRegistry">specifies a custom pickler registry.</param>
    [<Sealed>]
    [<AutoSerializable(false)>]
    type VagrantServer(?outpath : string, ?dynamicAssemblyProfiles : IDynamicAssemblyProfile list, ?picklerRegistry : CustomPicklerRegistry) =

        let outpath = 
            match outpath with
            | Some path when Directory.Exists path -> path
            | Some _ -> invalidArg "outPath" "not a valid directory."
            | None -> Path.GetTempPath()

        let dynamicAssemblyProfiles = 
            match dynamicAssemblyProfiles with
            | Some ps -> ps
            | None -> [ new FsiDynamicAssemblyProfile() :> IDynamicAssemblyProfile ]

        // initialize agents

        let compiler = mkCompilationAgent dynamicAssemblyProfiles outpath
        let pickler = mkFsPicklerInstance picklerRegistry (fun () -> compiler.CurrentState)
        let exporter = mkAssemblyExporter pickler (fun () -> compiler.CurrentState)
        let loader = new VagrantClient(pickler, Some compiler.CurrentState.ServerId)
        let compile (assemblies : Assembly list) = compiler.PostAndReply(assemblies).Value

        static let checkIsDynamic(a : Assembly) = 
            if a.IsDynamic then () 
            else invalidArg a.FullName "Vagrant: not a dynamic assembly"

        /// Unique identifier for the slice compiler
        member __.UUId = compiler.CurrentState.ServerId
        /// Returns the pickler used by the slice compiler
        member __.Pickler = pickler

        member __.Client = loader

        /// <summary>
        ///     Compiles slices for given dynamic assembly, if required.
        /// </summary>
        /// <param name="assembly">a dynamic assembly</param>
        member __.CompileDynamicAssemblySlice (assembly : Assembly) =
            do checkIsDynamic assembly
            compile [assembly] |> List.map (fun a -> a.Assembly)

//        /// <summary>
//        ///     Compiles slices for given dynamic assembly, if required.
//        /// </summary>
//        /// <param name="assembly">a dynamic assembly</param>
//        member __.CompileDynamicAssemblySlices (assemblies : seq<Assembly>) =
//            compile (Seq.toList assemblies)

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
            do checkIsDynamic assembly
            match compiler.CurrentState.DynamicAssemblies.TryFind assembly.FullName with
            | None -> []
            | Some info ->
                info.GeneratedSlices |> Seq.map (function KeyValue(_,s) -> s.Assembly) |> Seq.toList

//        /// <summary>
//        ///     Creates an exportable DependencyInfo bundle for given assembly.
//        ///     This includes potential type initialization data for dynamic assembly slices.
//        /// </summary>
//        /// <param name="assembly">a dunamic assembly slice</param>
//        member __.TryGetSliceInfo(assembly : Assembly) =
//            match compiler.CurrentState.TryFindSliceInfo assembly.FullName with
//            | None -> None
//            | Some (_,slice) -> Some slice


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
                let assemblies = getDynamicDependenciesRequiringCompilation compiler.CurrentState dependencies
                let _ = compile assemblies in ()

            remapDependencies compiler.CurrentState dependencies

        /// <summary>
        ///     Returns the dynamic assembly slice corresponding to the given type, if exists.
        /// </summary>
        /// <param name="t"></param>
        member __.TryGetSliceOfType(t : Type) =
            let t = if t.IsGenericType && not t.IsGenericTypeDefinition then t.GetGenericTypeDefinition() else t
            match compiler.CurrentState.DynamicAssemblies.TryFind t.Assembly.FullName with
            | None -> None
            | Some dyn -> dyn.TypeIndex.TryFind t.FullName |> Option.map (fun s -> s.Assembly)

        member __.MakePortableAssembly(assembly : Assembly, includeAssemblyImage:bool, includeStaticInitializers:bool) =
            exporter.PostAndReply(assembly, includeAssemblyImage, includeStaticInitializers)

        member __.SubmitAssemblies(submitF : PortableAssembly list -> Async<AssemblyLoadResponse list>, assemblies : Assembly list) =
            assemblySubmitProtocol exporter submitF assemblies

        member __.SubmitObjectDependencies(submitF : PortableAssembly list -> Async<AssemblyLoadResponse list>, obj:obj, ?permitCompilation) =
            let assemblies = __.ComputeObjectDependencies(obj, ?permitCompilation = permitCompilation)
            __.SubmitAssemblies(submitF, assemblies)
            