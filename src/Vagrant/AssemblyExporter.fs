module internal Nessos.Vagrant.AssemblyExporter

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils

    type StaticInitializers = (FieldInfo * Exn<byte []>) []

    /// export a portable assembly based on given compiler state and arguments

    let exportAssembly (pickler : FsPickler) (compilerState : DynamicAssemblyCompilerState) 
                        (state : Map<AssemblyId, int>) includeImage includeStaticInitializationData (assembly : Assembly) =

        let mkPortableAssembly generation dynamicAssemblyInfo =
            {
                Id = { assembly.AssemblyId with Generation = generation }
                DynamicAssemblyInfo = dynamicAssemblyInfo

                Image = if includeImage then Some <| File.ReadAllBytes assembly.Location else None
                Symbols =
                    if includeImage then
                        // TODO : mdb?
                        let symbolFile = Path.ChangeExtension(assembly.Location, "pdb")
                        if File.Exists symbolFile then
                            Some <| File.ReadAllBytes symbolFile
                        else
                            None
                    else
                        None
            }
                 
        match compilerState.TryFindSliceInfo assembly.FullName with
        | Some (dynAssembly, sliceInfo) ->

            let requiresStaticInitialization = sliceInfo.StaticFields.Length > 0
            let currentGeneration = defaultArg (state.TryFind assembly.AssemblyId) 0

            let isPartiallyEvaluated = 
                dynAssembly.Profile.IsPartiallyEvaluatedSlice 
                    (dynAssembly.TryGetSlice >> Option.map (fun s -> s.Assembly)) 
                        sliceInfo.Assembly

            let state, generation, staticInitializer =
                if requiresStaticInitialization && includeStaticInitializationData then

                    let tryPickle (fI : FieldInfo) =
                        try
                            let value = fI.GetValue(null)
                            let pickle = pickler.Pickle<obj>(value)
                            fI, Success pickle
                        with e -> 
                            fI, Error e

                    let staticInitializers = sliceInfo.StaticFields |> Array.map tryPickle
                    let data = pickler.Pickle<StaticInitializers>(staticInitializers)

                    let generation = currentGeneration + 1
                    let state = state.Add(assembly.AssemblyId, generation)
                    state, generation, Some (generation, data)
                else
                    state, currentGeneration, None

            let dynamicAssemblyInfo =
                {
                    SourceId = compilerState.ServerId
                    DynamicAssemblyName = sliceInfo.DynamicAssemblyQualifiedName
                    SliceId = sliceInfo.SliceId

                    RequiresStaticInitialization = requiresStaticInitialization
                    IsPartiallyEvaluated = isPartiallyEvaluated
                    StaticInitializerData = staticInitializer
                }

            state, mkPortableAssembly generation (Some dynamicAssemblyInfo)

        | _ -> state, mkPortableAssembly 0 None


    type AssemblyExporter = StatefulActor<Map<AssemblyId, int>, Assembly * bool * bool, PortableAssembly>

    let mkAssemblyExporter pickler (stateF : unit -> DynamicAssemblyCompilerState) : AssemblyExporter = 
        mkStatefulActor Map.empty (fun state (a,img,data) -> exportAssembly pickler (stateF()) state img data a)

    /// server-side protocol implementation

    let assemblySubmitProtocol (exporter : AssemblyExporter) (postWithReplyF : PortableAssembly list -> Async<AssemblyLoadResponse list>) (assemblies : Assembly list) =
        
        let throwOnError tolerateMissingResponses (replies : AssemblyLoadResponse list) =
            // check for faults first
            match replies |> List.tryPick (function LoadFault(n,e) -> Some(n,e) | _ -> None) with
            | Some(id, (:? VagrantException as e)) -> raise e
            | Some(id, e) -> raise <| new VagrantException(sprintf "error on remote loading of assembly '%s'." id.FullName, e)
            | None when tolerateMissingResponses -> ()
            | None ->
                match replies |> List.tryFind(function Loaded _ -> false | _ -> true) with
                | Some r -> raise <| new VagrantException(sprintf "protocol error, could not publish '%s'." r.Id.FullName)
                | None -> ()

        async {
            let index = assemblies |> Seq.map (fun a -> a.AssemblyId, a) |> Map.ofSeq

            // Step 1. submit empty assembly headers; no image data or type initializers.
            let headers = assemblies |> List.map (fun a -> exporter.PostAndReply(a, false, false))
            let! replies = postWithReplyF headers
        
            // Step 2. Detect faults, identify missing image or type initializers and repeat post with requested data

            let updatePortableAssembly = 
                function
                | Loaded _ | LoadFault _ -> None
                | MissingAssemblyImage id -> Some <| exporter.PostAndReply(index.[id], true, true)
                | MissingStaticInitializer(id,_) -> Some <| exporter.PostAndReply(index.[id], false, true)

            do throwOnError true replies
            let portableAssemblies = replies |> List.choose updatePortableAssembly
            let! replies = postWithReplyF portableAssemblies

            // Step 3. check results of second attempt; if client still replies negatively, fail
            do throwOnError false replies

            let staticInitializationErrors = replies |> Seq.collect (fun r -> r.StaticInitializationErrors) |> Seq.toArray
            return staticInitializationErrors
        }