namespace Nessos.Vagrant
    
    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.Serialization
    open Nessos.Vagrant.DependencyAnalysis
    open Nessos.Vagrant.SliceCompiler
    open Nessos.Vagrant.AssemblyExporter
    open Nessos.Vagrant.AssemblyLoader

    /// <summary>
    ///     Client for loading type initialization blobs of dynamic assembly slices.
    /// </summary>
    [<Sealed>]
    [<AutoSerializable(false)>]
    type VagrantClient internal (pickler : FsPickler, isLocalSlice : AssemblyId -> bool) =

        static do registerAssemblyResolutionHandler()

        let loader = mkAssemblyLoader pickler isLocalSlice

        /// <summary>
        ///     Client for loading type initialization blobs of dynamic assembly slices.
        /// </summary>
        /// <param name="pickler">Specify a custom pickler instance.</param>
        new (?pickler : FsPickler) =
            let pickler = match pickler with None -> new FsPickler() | Some p -> p
            new VagrantClient(pickler, fun _ -> false)

        /// Returns pickler used in type initialization.
        member __.Pickler = pickler

        /// <summary>
        ///     Get the current assembly load state for given id.
        /// </summary>
        /// <param name="id"></param>
        member __.GetAssemblyLoadInfo(id : AssemblyId) = getAssemblyLoadInfo loader id

        /// <summary>
        ///     Loads the type initializers from given dependency package.
        /// </summary>
        /// <param name="assembly">Portable assembly package to be loaded.</param>
        member __.LoadPortableAssembly(assembly : PortableAssembly) = loader.PostAndReply assembly

        /// <summary>
        ///     Loads the type initializers from given dependency packages.
        /// </summary>
        /// <param name="assemblies">Portable assembly packages to be loaded.</param>
        member __.LoadPortableAssemblies(assemblies : seq<PortableAssembly>) =
            assemblies |> Seq.map loader.PostAndReply |> Seq.toList

        /// <summary>
        ///     Receive dependencies as supplied by the remote assembly publisher
        /// </summary>
        /// <param name="publisher">The remote publisher</param>
        member __.ReceiveDependencies(publisher : IRemoteAssemblyPublisher) =
            assemblyReceiveProtocol loader publisher
            


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
        let loader = new VagrantClient(pickler, fun id -> compiler.CurrentState.TryGetDynamicAssemblyId(id.FullName).IsSome)
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
            | Some dyn -> dyn.TryGetSlice t |> Option.map (fun s -> s.Assembly)

        /// <summary>
        ///     Builds a portable assembly bundle for given input.
        /// </summary>
        /// <param name="assembly">Given assembly.</param>
        /// <param name="includeAssemblyImage">Include raw assembly image in the bundle.</param>
        member __.MakePortableAssembly(assembly : Assembly, includeAssemblyImage:bool) =
            exporter.PostAndReply(assembly, includeAssemblyImage)

        /// <summary>
        ///     Apply the built-in assembly distribution protocol using user-defined submit function.
        /// </summary>
        /// <param name="receiver">User provided assembly submit operation.</param>
        /// <param name="assemblies">Assemblies to be exported.</param>
        member __.SubmitAssemblies(receiver : IRemoteAssemblyReceiver, assemblies : Assembly list) =
            for a in assemblies do
                if a.IsDynamic then
                    invalidArg a.FullName "cannot submit dynamic assemblies."

            assemblySubmitProtocol exporter receiver assemblies

        /// <summary>
        ///     Apply the built-in assembly distribution protocol using user-defined function.
        /// </summary>
        /// <param name="receiver">User provided assembly submit operation.</param>
        /// <param name="obj">Object, whose dependent assemblies are to be exported.</param>
        /// <param name="permitCompilation">Compile dynamic assemblies in the background, as required. Defaults to false.</param>
        member __.SubmitObjectDependencies(receiver : IRemoteAssemblyReceiver, obj:obj, ?permitCompilation) =
            let assemblies = __.ComputeObjectDependencies(obj, ?permitCompilation = permitCompilation)
            __.SubmitAssemblies(receiver, assemblies)
            