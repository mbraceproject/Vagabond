namespace Nessos.Vagrant

    open System
    open System.Reflection

    open Nessos.Vagrant.DependencyAnalysis

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

