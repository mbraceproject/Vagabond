module internal Nessos.Vagrant.AssemblyExporter

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.SliceCompilerTypes
    open Nessos.Vagrant.Utils

    type StaticInitializers = (FieldInfo * Exn<byte []>) []

    /// export a portable assembly based on given compiler state and arguments

    let exportAssembly (pickler : FsPickler) (compilerState : DynamicAssemblyCompilerState) 
                        (generationIndex : Map<AssemblyId, int>) (includeImage : bool) (assembly : Assembly) =

        let id = assembly.AssemblyId

        let mkPortableAssembly sliceInfo staticInitializer =
            let image = if includeImage then Some <| File.ReadAllBytes assembly.Location else None
            let symbols =
                if includeImage then
                    // TODO : mdb?
                    let symbolFile = Path.ChangeExtension(assembly.Location, "pdb")
                    if File.Exists symbolFile then
                        Some <| File.ReadAllBytes symbolFile
                    else
                        None
                else
                    None

            let info = 
                {
                    Id = id

                    IsImageLoaded = image.IsSome
                    IsSymbolsLoaded = symbols.IsSome

                    DynamicAssemblySliceInfo = sliceInfo
                }
                
            {
                Info = info
                Image = image
                Symbols = symbols
                StaticInitializer = staticInitializer
            }

        match compilerState.TryFindSliceInfo id.FullName with
        | Some (dynAssembly, sliceInfo) ->

            let mkDynamicAssemblyInfo reqStatic gen isPartial =
                {
                    SourceId = compilerState.ServerId
                    DynamicAssemblyQualifiedName = sliceInfo.DynamicAssemblyQualifiedName
                    SliceId = sliceInfo.SliceId

                    RequiresStaticInitialization = reqStatic
                    StaticInitializerGeneration = gen
                    IsPartiallyEvaluatedStaticInitializer = isPartial
                }

            if sliceInfo.RequiresStaticInitialization then
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

                let info = mkDynamicAssemblyInfo true generation isPartiallyEvaluated
                let generationIndex = generationIndex.Add(id, generation)

                generationIndex, mkPortableAssembly (Some info) (Some (generation,data))
            else
                let info = mkDynamicAssemblyInfo false 0 false
                generationIndex, mkPortableAssembly (Some info) None

        | _ -> generationIndex, mkPortableAssembly None None


    type AssemblyExporter = StatefulActor<Map<AssemblyId, int>, Assembly * bool, PortableAssembly>

    let mkAssemblyExporter pickler (stateF : unit -> DynamicAssemblyCompilerState) : AssemblyExporter = 
        mkStatefulActor Map.empty (fun state (a,includeImg) -> exportAssembly pickler (stateF()) state includeImg a)





    /// server-side protocol implementation

    let assemblySubmitProtocol (exporter : AssemblyExporter) (receiver : IRemoteAssemblyReceiver) (assemblies : Assembly list) =
        
//        let throwOnError tolerateMissingResponses (replies : AssemblyLoadResponse list) =
//            // check for faults first
//            match replies |> List.tryPick (function LoadFault(n,e) -> Some(n,e) | _ -> None) with
//            | Some(id, (:? VagrantException as e)) -> raise e
//            | Some(id, e) -> raise <| new VagrantException(sprintf "error on remote loading of assembly '%s'." id.FullName, e)
//            | None when tolerateMissingResponses -> ()
//            | None ->
//                match replies |> List.tryFind(function Loaded _ -> false | _ -> true) with
//                | Some r -> raise <| new VagrantException(sprintf "protocol error, could not publish '%s'." r.Id.FullName)
//                | None -> ()

        async {
            let index = assemblies |> Seq.map (fun a -> a.AssemblyId, a) |> Map.ofSeq

            // Step 1. submit assembly identifiers to receiver; get assembly state
            let headers = assemblies |> List.map (fun a -> a.AssemblyId)
            let! info = receiver.GetLoadedAssemblyInfo headers
        
            // Step 2. Detect faults, identify missing image or type initializers and repeat post with requested data
            let tryGetPortableAssembly (info : AssemblyInfo) =
                match info.DynamicAssemblySliceInfo with
                | None when info.IsImageLoaded -> None
                | None -> Some <| exporter.PostAndReply(index.[info.Id], true)
                | Some dyn when info.IsImageLoaded && not dyn.IsPartiallyEvaluatedStaticInitializer -> None
                | Some dyn -> Some <| exporter.PostAndReply(index.[info.Id], info.IsImageLoaded)

            let portableAssemblies = info |> List.choose tryGetPortableAssembly
            let! replies = receiver.PushAssemblies portableAssemblies

            // Step 3. check load results; if client replies with fault, fail.
            let gatherErrors = 
                function
                | LoadSuccess(_,errors) -> errors
                | LoadFault(_, (:? VagrantException as e)) -> raise e
                | LoadFault(id, e) -> raise <| new VagrantException(sprintf "error on remote loading of assembly '%s'." id.FullName)

            let staticInitializationErrors = replies |> List.map gatherErrors |> Array.concat
            return staticInitializationErrors
        }