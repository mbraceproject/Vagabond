namespace Nessos.Vagrant

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.DependencyAnalysis

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



//    /// <summary>
//    ///     Persist assemblies and Vagrant-related metadata to disk.
//    /// </summary>
//    type AssemblyCache(pickler : FsPickler, cacheDirectory : string) =
//        do 
//            if not <| Directory.Exists cacheDirectory then
//                raise <| new DirectoryNotFoundException(cacheDirectory)
//
//        let getFilePath (assembly : AssemblyId) =
//            let hash = Convert.toBase32String assembly.ImageHash
//            let fileName = sprintf "%s-%s.dll" (assembly.GetName().Name) hash
//            Path.Combine(cacheDirectory, fileName)
//
//        let readDynamicAssemblyInfo path =
//            use fs = new FileStream(path, FileMode.Open)
//            pickler.Deserialize<DynamicAssemblyInfo>(fs)
//
//        let writeDynamicAssemblyInfo path info =
//            use fs = new FileStream(path, FileMode.Create)
//            pickler.Serialize<DynamicAssemblyInfo>(fs, info)
//        
//        /// <summary>
//        ///     Save given portable assembly to cache
//        /// </summary>
//        /// <param name="assembly">The assembly to persist.</param>
//        member __.Cache(assembly : PortableAssembly) =
//            let dll = getFilePath assembly.Id
//            let pdb = Path.ChangeExtension(dll, ".pdb")
//            let metadata = Path.ChangeExtension(dll, ".vagrant")
//            
//            // cache the file
//            if File.Exists dll then ()
//            else
//                match assembly.Image with
//                | None -> invalidOp <| sprintf "AssemblyCache: Portable assembly '%s' missing image." assembly.FullName
//                | Some img -> File.WriteAllBytes(dll, img)
//
//            // cache the symbols
//            if File.Exists pdb then ()
//            else
//                match assembly.Symbols with
//                | None -> ()
//                | Some symbols -> File.WriteAllBytes(pdb, symbols)
//
//            // cache metadata
//            match assembly.DynamicAssemblyInfo with
//            | None -> ()
//            | Some info ->
//                if File.Exists metadata then
//                    let pastInfo = readDynamicAssemblyInfo metadata
//                    if pastInfo.SliceId < info.SliceId then
//                        writeDynamicAssemblyInfo metadata info
//                else
//                    writeDynamicAssemblyInfo metadata info
//
//        /// <summary>
//        ///     Loads given portable assembly from cache, if it exists.
//        /// </summary>
//        /// <param name="id">Provided assembly id.</param>
//        /// <param name="includeImage">Specifies whether to include image. Defaults to true.</param>
//        /// <param name="includeMetadata">Specifies whether to include vagrant metadata. Defaults to true.</param>
//        member __.TryGetCachedAssembly (id : AssemblyId, ?includeImage, ?includeMetadata) =
//            let includeImage = defaultArg includeImage true
//            let includeMetadata = defaultArg includeMetadata true
//
//            let dll = getFilePath id
//            let pdb = Path.ChangeExtension(dll, ".pdb")
//            let metadata = Path.ChangeExtension(dll, ".vagrant")
//
//            if not <| File.Exists dll then None else
//
//            let image =
//                if includeImage then
//                    Some <| File.ReadAllBytes dll
//                else
//                    None
//
//            let symbols =
//                if includeImage && File.Exists pdb then
//                    Some <| File.ReadAllBytes pdb
//                else
//                    None
//
//            let metadata =
//                if includeMetadata && File.Exists metadata then
//                    Some <| readDynamicAssemblyInfo metadata
//                else
//                    None
//
//            Some <| { Id = id ; Image = image ; Symbols = symbols ; DynamicAssemblyInfo = metadata }
//
//        /// <summary>
//        ///     Determines whether provided assembly exists in cache.
//        /// </summary>
//        /// <param name="id">Assembly id.</param>
//        member __.IsCachedAssembly (id : AssemblyId) =
//            let dll = getFilePath id
//            File.Exists dll