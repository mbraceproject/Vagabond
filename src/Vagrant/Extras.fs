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
    type VagrantCache(cacheDirectory : string, ?pickler : FsPickler, ?lookupAppDomain) =
        do 
            if not <| Directory.Exists cacheDirectory then
                raise <| new DirectoryNotFoundException(cacheDirectory)


        let pickler = match pickler with Some p -> p | None -> new FsPickler()
        let lookupAppDomain = defaultArg lookupAppDomain false
        let cacheActor = initAssemblyCache pickler lookupAppDomain cacheDirectory
        
        /// <summary>
        ///     Save given portable assembly to cache
        /// </summary>
        /// <param name="assembly">The assembly to persist.</param>
        member __.Cache(assembly : PortableAssembly) = cacheActor.PostAndReply assembly

        /// <summary>
        ///     Save given portable assemblies to cache
        /// </summary>
        /// <param name="assemblies"></param>
        member __.Cache(assemblies : PortableAssembly list) = List.map cacheActor.PostAndReply assemblies

        /// directory used by the cache
        member __.CacheDirectory = cacheDirectory

        /// <summary>
        ///     Loads given portable assembly from cache, if it exists.
        /// </summary>
        /// <param name="id">Provided assembly id.</param>
        /// <param name="includeImage">Specifies whether to include image. Defaults to true.</param>
        member __.TryGetCachedAssembly (id : AssemblyId, ?includeImage) =
            let includeImage = defaultArg includeImage true
            tryGetPortableAssemblyFromCache cacheActor cacheDirectory includeImage id

        /// <summary>
        ///     Loads given portable assembly from cache, if it exists.
        /// </summary>
        /// <param name="id">Provided assembly id.</param>
        /// <param name="includeImage">Specifies whether to include image. Defaults to true.</param>        
        member __.GetCachedAssembly (id : AssemblyId, ?includeImage) =
            match __.TryGetCachedAssembly(id, ?includeImage = includeImage) with
            | None -> raise <| new VagrantException(sprintf "could not load '%s' from cache" id.FullName)
            | Some pa -> pa

        /// <summary>
        ///     Retrieves assembly cache information
        /// </summary>
        /// <param name="id">Assembly id.</param>
        member __.GetCachedAssemblyInfo (id : AssemblyId) =
            cacheActor.PostAndReply <| PortableAssembly.Empty id

        /// <summary>
        ///     Retrieves assembly cache information for given id's.
        /// </summary>
        /// <param name="ids">Assembly id's.</param>
        member __.GetCachedAssemblyInfo (ids : AssemblyId list) =
            List.map (cacheActor.PostAndReply << PortableAssembly.Empty) ids

        /// <summary>
        ///     Determines whether assembly is cached.
        /// </summary>
        /// <param name="id">Assembly id.</param>
        member __.IsCachedAssembly (id : AssemblyId) =
            match __.GetCachedAssemblyInfo id with
            | Loaded _ | LoadedWithStaticIntialization _ -> true
            | _ -> false

        /// <summary>
        ///     Get currently loaded assembly state.
        /// </summary>
        member __.CachedAssemblies = 
            cacheActor.CurrentState 
            |> Seq.map(function (KeyValue(_,(_,info))) -> info) 
            |> Seq.toList