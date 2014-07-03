module internal Nessos.Vagrant.AssemblyExporter

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.SliceCompilerTypes
    open Nessos.Vagrant.AssemblyStore

    type ExporterState = Map<AssemblyId, int>
    type StaticInitializers = (FieldInfo * Exn<byte []>) []

//    let mkPortableAssembly includeImage staticInitializer (assembly : Assembly) =
//        let image = if includeImage then Some <| File.ReadAllBytes assembly.Location else None
//        let symbols =
//            if includeImage then
//                // TODO : mdb?
//                let symbolFile = Path.ChangeExtension(assembly.Location, "pdb")
//                if File.Exists symbolFile then
//                    Some <| File.ReadAllBytes symbolFile
//                else
//                    None
//            else
//                None
//                
//        {
//            Id = assembly.AssemblyId
//            Image = image
//            Symbols = symbols
//            StaticInitializer = staticInitializer
//        }

    /// export a portable assembly based on given compiler state and arguments

    let exportAssembly (pickler : BasePickler) (store : AssemblyStore)
                        (compilerState : DynamicAssemblyCompilerState) (generationIndex : ExporterState) 
                        (includeImage : bool) (id : AssemblyId) =

        match compilerState.TryFindSliceInfo id.FullName with
        | Some (dynAssembly, sliceInfo) when sliceInfo.RequiresStaticInitialization ->

            let generation = 1 + defaultArg (generationIndex.TryFind id) -1
                
            let tryPickle (fI : FieldInfo) =
                try
                    let value = fI.GetValue(null)
                    let pickle = pickler.Pickle<obj>(value)
                    fI, Success pickle
                with e -> 
                    fI, Error e

            let staticInitializers = Array.map tryPickle sliceInfo.StaticFields
            let data = pickler.Pickle<StaticInitializers>(staticInitializers)

            let isPartiallyEvaluated = 
                dynAssembly.Profile.IsPartiallyEvaluatedSlice
                    (dynAssembly.TryGetSlice >> Option.map (fun s -> s.Assembly)) 
                        sliceInfo.Assembly

            let staticInitializer =
                {
                    Generation = generation
                    IsPartial = isPartiallyEvaluated
                    Data = data
                }

            let generationIndex = generationIndex.Add(id, generation)
            let pa = store.CreatePortableAssembly(sliceInfo.Assembly, includeImage = includeImage)
            generationIndex, { pa with StaticInitializer = Some staticInitializer }

        | Some(_, sliceInfo) -> generationIndex, store.CreatePortableAssembly(sliceInfo.Assembly, includeImage = includeImage)

        | None ->
            match tryGetLoadedAssembly id.FullName with
            | Some assembly -> generationIndex, store.CreatePortableAssembly(assembly, includeImage = includeImage)
            | None ->
                match store.TryGetCachedAssemblyInfo id with
                | Some info -> generationIndex, store.CreatePortableAssembly(info, includeImage = includeImage)
                | None -> invalidOp "could not find assembly location."


//    type AssemblyExporter = StatefulActor<Map<AssemblyId, int>, Assembly * bool, PortableAssembly>
//
//    let mkAssemblyExporter pickler (stateF : unit -> DynamicAssemblyCompilerState) : AssemblyExporter = 
//        mkStatefulActor Map.empty (fun state (a,includeImg) -> exportAssembly pickler (stateF()) state includeImg a)
//
//
//
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