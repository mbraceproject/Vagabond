namespace Nessos.Vagrant

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.DependencyAnalysis
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
    ///     Persist assemblies and Vagrant-related metadata to disk.
    /// </summary>
    type VagrantCache(pickler : FsPickler, cacheDirectory : string) =
        do 
            if not <| Directory.Exists cacheDirectory then
                raise <| new DirectoryNotFoundException(cacheDirectory)


        let cacheActor = initAssemblyCache pickler cacheDirectory
        
        /// <summary>
        ///     Save given portable assembly to cache
        /// </summary>
        /// <param name="assembly">The assembly to persist.</param>
        member __.Cache(assembly : PortableAssembly) = cacheActor.PostAndReply assembly

        /// <summary>
        ///     Loads given portable assembly from cache, if it exists.
        /// </summary>
        /// <param name="id">Provided assembly id.</param>
        /// <param name="includeImage">Specifies whether to include image. Defaults to true.</param>
        /// <param name="includeMetadata">Specifies whether to include vagrant metadata. Defaults to true.</param>
        member __.TryGetCachedAssembly (id : AssemblyId, ?includeImage) =
            let includeImage = defaultArg includeImage true
            tryGetPortableAssemblyFromCache cacheActor cacheDirectory includeImage id

        /// <summary>
        ///     Determines whether provided assembly exists in cache.
        /// </summary>
        /// <param name="id">Assembly id.</param>
        member __.GetCachedAssemblyInfo (id : AssemblyId) =
            cacheActor.PostAndReply <| PortableAssembly.Empty id