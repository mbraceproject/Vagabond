namespace Nessos.Vagrant

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.DependencyAnalysis
    open Nessos.Vagrant.AssemblyExporter
    open Nessos.Vagrant.AssemblyCache

    /// <summary>
    ///     A collection of general purpose utilities used by Vagrant
    /// </summary>
    type VagrantUtils =

        /// <summary>
        ///     Returns all type instances that appear in given object graph.
        /// </summary>
        /// <param name="obj">object graph to be traversed</param>
        static member ComputeTypeDependencies(obj:obj) : Type [] = gatherObjectDependencies obj |> fst

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


        /// <summary>
        ///     Computes a unique id for given static assembly.
        /// </summary>
        /// <param name="assembly">a static assembly.</param>
        static member ComputeAssemblyId (assembly : Assembly) = 
            let _ = assembly.Location // force exception in case of dynamic assembly
            assembly.AssemblyId


        /// <summary>
        ///     Creates a portable assembly package.
        /// </summary>
        /// <param name="assembly">input assembly</param>
        static member CreatePortableAssembly (assembly : Assembly) =
            mkPortableAssembly true None assembly



    /// <summary>
    ///     Persist assemblies and Vagrant-related metadata to disk.
    /// </summary>
    /// <param name="cacheDirectory">Local directory for caching assemblies.</param>
    /// <param name="pickler">Custom FsPickler instance.</param>
    /// <param name="requireIdentical">Require that loaded assembly must be of identical SHA256 hashcode. Defaults to false.</param>
    /// <param name="loadPolicy">Specifies local assembly resolution policy. Defaults to strong names only.</param>
    type VagrantCache(cacheDirectory : string, ?pickler : BasePickler, ?loadPolicy, ?requireIdentical) =
        do 
            if not <| Directory.Exists cacheDirectory then
                raise <| new DirectoryNotFoundException(cacheDirectory)

        let _loadPolicy = defaultArg loadPolicy AssemblyLocalResolutionPolicy.StrongNamesOnly
        let _requireIndetical = defaultArg requireIdentical true


        let pickler = match pickler with Some p -> p | None -> FsPickler.CreateBinary() :> _
        let cacheActor = initAssemblyCache pickler cacheDirectory

        let cacheAssembly requireIdentical loadPolicy pa =
            cacheActor.PostAndReply(pa, 
                defaultArg requireIdentical _requireIndetical, 
                defaultArg loadPolicy _loadPolicy)
        
        /// <summary>
        ///     Save given portable assembly to cache
        /// </summary>
        /// <param name="assembly">The assembly to persist.</param>
        /// <param name="requireIdentical">Require that loaded assembly must be of identical SHA256 hashcode. Defaults to false.</param>
        /// <param name="loadPolicy">Specifies local assembly resolution policy. Defaults to strong names only.</param>
        member __.Cache(assembly : PortableAssembly, ?requireIdentical, ?loadPolicy) =
            cacheAssembly requireIdentical loadPolicy assembly

        /// <summary>
        ///     Save given portable assemblies to cache
        /// </summary>
        /// <param name="assemblies"></param>
        /// <param name="requireIdentical">Require that loaded assembly must be of identical SHA256 hashcode. Defaults to false.</param>
        /// <param name="loadPolicy">Specifies local assembly resolution policy. Defaults to strong names only.</param>
        member __.Cache(assemblies : PortableAssembly list, ?requireIdentical, ?loadPolicy) =
            List.map (cacheAssembly requireIdentical loadPolicy) assemblies

        /// directory used by the cache
        member __.CacheDirectory = cacheDirectory

        /// <summary>
        ///     Loads given portable assembly from cache, if it exists.
        /// </summary>
        /// <param name="id">Provided assembly id.</param>
        /// <param name="includeImage">Specifies whether to include image. Defaults to true.</param>
        /// <param name="requireIdentical">Require that loaded assembly must be of identical SHA256 hashcode. Defaults to false.</param>
        /// <param name="loadPolicy">Specifies local assembly resolution policy. Defaults to strong names only.</param>
        member __.TryGetCachedAssembly (id : AssemblyId, ?includeImage, ?requireIdentical, ?loadPolicy) =
            let includeImage = defaultArg includeImage true
            let loadPolicy = defaultArg loadPolicy _loadPolicy
            let requireIdentical = defaultArg requireIdentical _requireIndetical
            tryGetPortableAssemblyFromCache cacheActor cacheDirectory 
                includeImage requireIdentical loadPolicy id

        /// <summary>
        ///     Loads given portable assembly from cache, if it exists.
        /// </summary>
        /// <param name="id">Provided assembly id.</param>
        /// <param name="includeImage">Specifies whether to include image. Defaults to true.</param>      
        /// <param name="requireIdentical">Require that loaded assembly must be of identical SHA256 hashcode. Defaults to false.</param>
        /// <param name="loadPolicy">Specifies local assembly resolution policy. Defaults to strong names only.</param>  
        member __.GetCachedAssembly (id : AssemblyId, ?includeImage, ?requireIdentical, ?loadPolicy) =
            match __.TryGetCachedAssembly(id, ?includeImage = includeImage, ?requireIdentical = requireIdentical, ?loadPolicy = loadPolicy) with
            | None -> raise <| new VagrantException(sprintf "could not load '%s' from cache" id.FullName)
            | Some pa -> pa

        /// <summary>
        ///     Retrieves assembly cache information
        /// </summary>
        /// <param name="id">Assembly id.</param>
        /// <param name="requireIdentical">Require that loaded assembly must be of identical SHA256 hashcode. Defaults to false.</param>
        /// <param name="loadPolicy">Specifies local assembly resolution policy. Defaults to strong names only.</param>  
        member __.GetCachedAssemblyInfo (id : AssemblyId, ?requireIdentical, ?loadPolicy) =
            cacheAssembly requireIdentical loadPolicy (PortableAssembly.Empty id)

        /// <summary>
        ///     Retrieves assembly cache information for given id's.
        /// </summary>
        /// <param name="ids">Assembly id's.</param>
        /// <param name="requireIdentical">Require that loaded assembly must be of identical SHA256 hashcode. Defaults to false.</param>
        /// <param name="loadPolicy">Specifies local assembly resolution policy. Defaults to strong names only.</param>  
        member __.GetCachedAssemblyInfo (ids : AssemblyId list, ?requireIdentical, ?loadPolicy) =
            List.map (cacheAssembly requireIdentical loadPolicy << PortableAssembly.Empty) ids

        /// <summary>
        ///     Determines whether assembly is cached.
        /// </summary>
        /// <param name="id">Assembly id.</param>
        /// <param name="requireIdentical">Require that loaded assembly must be of identical SHA256 hashcode. Defaults to false.</param>
        /// <param name="loadPolicy">Specifies local assembly resolution policy. Defaults to strong names only.</param>  
        member __.IsCachedAssembly (id : AssemblyId, ?requireIdentical, ?loadPolicy) =
            match __.GetCachedAssemblyInfo (id, ?requireIdentical = requireIdentical, ?loadPolicy = loadPolicy) with
            | Loaded _ | LoadedWithStaticIntialization _ -> true
            | _ -> false

        /// <summary>
        ///     Get currently loaded assembly state.
        /// </summary>
        member __.CachedAssemblies = 
            cacheActor.CurrentState 
            |> Seq.map(function (KeyValue(_,(_,info))) -> info) 
            |> Seq.toList

//
//
//    /// server-side protocol implementation
//
//    let assemblySubmitProtocol (exporter : AssemblyExporter) (receiver : IRemoteAssemblyReceiver) (assemblies : Assembly list) =
//
//        async {
//            let index = assemblies |> Seq.map (fun a -> a.AssemblyId, a) |> Map.ofSeq
//
//            // Step 1. submit assembly identifiers to receiver; get back loaded state
//            let headers = assemblies |> List.map (fun a -> a.AssemblyId)
//            let! info = receiver.GetLoadedAssemblyInfo headers
//        
//            // Step 2. detect dependencies that require posting
//            let tryGetPortableAssembly (info : AssemblyLoadInfo) =
//                match info with
//                | LoadFault(id, (:?VagrantException as e)) -> raise e
//                | LoadFault(id, e) -> 
//                    raise <| new VagrantException(sprintf "error on remote loading of assembly '%s'." id.FullName)
//                | NotLoaded id -> 
//                    Some <| exporter.PostAndReply(index.[id], true)
//                | Loaded _ -> None
//                | LoadedWithStaticIntialization(id, si) when si.IsPartial ->
//                    Some <| exporter.PostAndReply(index.[info.Id], false)
//                | LoadedWithStaticIntialization _ -> None
//                
//
//            let portableAssemblies = info |> List.choose tryGetPortableAssembly
//            let! loadResults = receiver.PushAssemblies portableAssemblies
//
//            // Step 3. check load results; if client replies with fault, fail.
//            let gatherErrors (info : AssemblyLoadInfo) =
//                match info with
//                | LoadFault(id, (:?VagrantException as e)) -> raise e
//                | LoadFault(id, _)
//                | NotLoaded id -> raise <| new VagrantException(sprintf "could not load assembly '%s' on remote client." id.FullName)
//                | Loaded _ -> None
//                | LoadedWithStaticIntialization(_,info) -> Some info.Errors
//
//            let staticInitializationErrors = loadResults |> List.choose gatherErrors |> Array.concat
//            return staticInitializationErrors
//        }



//    /// assembly receive protocol on the client side
//    let assemblyReceiveProtocol (loader : AssemblyLoader) requireIdentical loadPolicy (publisher : IRemoteAssemblyPublisher) = async {
//        // step 1. download dependencies required by publisher
//        let! dependencies = publisher.GetRequiredAssemblyInfo()
//
//        // step 2. resolve dependencies that are missing from client
//        let tryCheckLoadStatus (id : AssemblyId) =
//            match getAssemblyLoadInfo loader requireIdentical loadPolicy id with
//            | NotLoaded id
//            | LoadFault (id,_) -> Some id
//            | Loaded _ -> None
//            | LoadedWithStaticIntialization _ -> None
//
//        let missing = dependencies |> List.choose tryCheckLoadStatus
//
//        if missing.Length > 0 then
//            // step 3. download portable assemblies for missing dependencies
//            let! assemblies = publisher.PullAssemblies missing
//            let loadResults = assemblies |> List.map (fun a -> loader.PostAndReply(a, requireIdentical, loadPolicy))
//
//            let checkLoadResult (info : AssemblyLoadInfo) =
//                match info with
//                | NotLoaded id -> raise <| new VagrantException(sprintf "failed to load assembly '%s'" id.FullName)
//                | LoadFault(_, (:? VagrantException as e)) -> raise e
//                | LoadFault(id, e) -> raise <| new VagrantException(sprintf "failed to load assembly '%s'" id.FullName, e)
//                | Loaded _ | LoadedWithStaticIntialization _ -> ()
//
//            List.iter checkLoadResult loadResults
//    }