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


    //
    // assembly loader protocol implementation
    //

    type LoadedAssemblyInfo =
        | UnLoaded
        | LoadedStatic of AssemblyId
        | LoadedDynamic of requiresStaticInitialization:bool * generation:int option * isPartiallyEvaluated:bool


    /// need an assembly resolution handler when loading assemblies at runtime

    let registerAssemblyResolutionHandler () = 
        System.AppDomain.CurrentDomain.add_AssemblyResolve <| 
            new ResolveEventHandler (fun _ args -> defaultArg (tryGetLoadedAssembly args.Name) null)

    /// portable assembly load protocol implementation

    let rec loadAssembly (pickler : FsPickler) (localId : Guid option) (state : Map<string, LoadedAssemblyInfo>) (pa : PortableAssembly) =

        // load assembly image
        let tryLoadAssembly (pa : PortableAssembly) =
            match tryLoadAssembly pa.FullName with
            | Some a when a.AssemblyId = pa.Id -> Some a
            | Some _ -> raise <| new VagrantException(sprintf "an incompatible version of '%s' has been loaded in the client." pa.FullName)
            | None ->
                // try load binary image
                match pa.Image with
                | None -> None
                | Some bytes ->
                    let assembly =
                        try
                            match pa.Symbols with
                            | None -> System.Reflection.Assembly.Load(bytes)
                            | Some symbols -> System.Reflection.Assembly.Load(bytes, symbols)

                        with e -> raise <| new VagrantException(sprintf "Error loading assembly '%s.'" pa.FullName, e)

                    if assembly.FullName <> pa.FullName then
                        let msg = sprintf "Expected assembly '%s', received '%s'." pa.FullName assembly.FullName
                        raise <| new VagrantException(msg)

                    Some assembly

        let loadStaticInitializer (data : byte []) =
            let tryLoad (fI : FieldInfo, data : Exn<byte []>) =
                match data with
                | Success bytes ->
                    try
                        let value = pickler.UnPickle<obj> bytes
                        fI.SetValue(null, value) ; None
                    with e -> Some(fI, e)
                | Error e -> Some(fI, e)

            let initializers = pickler.UnPickle<StaticInitializers>(data)
            Array.choose tryLoad initializers
           
        let updateState (pa : PortableAssembly) status = state.Add(pa.FullName, status)
        let currentStatus = defaultArg (state.TryFind pa.FullName) UnLoaded

        try
            match localId, currentStatus, pa.DynamicAssemblyInfo with
            // static assembly already loaded in state
            | _, LoadedStatic id', _ when pa.Id = id' -> state, Loaded (pa.Id, [||])
            | _, LoadedStatic _, _ -> raise <| new VagrantException(sprintf "an incompatible version of '%s' has been loaded in the client." pa.FullName)

            // static assembly not registered in state
            | _, UnLoaded, None ->
                match tryLoadAssembly pa with
                | None -> state, MissingAssemblyImage pa.Id
                | Some a ->
                    updateState pa (LoadedStatic pa.Id), Loaded (pa.Id, [||])

            // trying to load local dynamic assembly
            | Some localId, _, Some dynInfo when dynInfo.SourceId = localId -> state, Loaded(pa.Id, [||])
            // dynamic assembly slice, not already loaded in state
            | _, UnLoaded, Some info ->
                match tryLoadAssembly pa with
                | None -> state, MissingAssemblyImage pa.Id
                | Some a ->
                    // assembly loaded in AppDomain, update state and recurse
                    let state = updateState pa <| LoadedDynamic(info.RequiresStaticInitialization, None, info.IsPartiallyEvaluated)
                    loadAssembly pickler localId state pa

            // loaded dynamic assemblies that do not require static initializers
            | _, LoadedDynamic(requiresStaticInitialization = false), _ -> state, Loaded(pa.Id, [||])

            // loaded dynamic assemblies without dynamic assembly info
            | _, LoadedDynamic(requiresStaticInitialization = true ; generation = gen ; isPartiallyEvaluated = isPartial), None ->
                match gen with
                | None -> state, MissingStaticInitializer(pa.Id, None)
                | Some g ->
                    if isPartial || g < pa.Id.Generation then
                        state, MissingStaticInitializer(pa.Id, None)
                    else
                        state, Loaded(pa.Id, [||])

            // loaded dynamic assembly slices without any previous initialization data
            | _, LoadedDynamic(requiresStaticInitialization = true ; generation = None), Some info ->
                match info.StaticInitializerData with
                | None -> state, MissingStaticInitializer (pa.Id, None)
                | Some (gen, data) ->
                    let errors = loadStaticInitializer data
                    let state = updateState pa <| LoadedDynamic(true, Some gen, info.IsPartiallyEvaluated)
                    state, Loaded(pa.Id, errors)

            // load type initializers when previous initialization is present
            | _, LoadedDynamic(requiresStaticInitialization = true ; generation = Some previousGen ; isPartiallyEvaluated = isPartial), Some current ->
                // silently discard updates if generation is older than currently loaded
                if previousGen > pa.Id.Generation then state, Loaded (pa.Id, [||])
                else
                    match current.StaticInitializerData with
                    // demand static initializer update if previous push was declared as partial
                    | None when isPartial -> state, MissingStaticInitializer (pa.Id, Some previousGen)
                    | None -> state, Loaded (pa.Id, [||])
                    | Some (gen, data) ->
                        let errors = loadStaticInitializer data
                        let state = updateState pa <| LoadedDynamic(true, Some gen, current.IsPartiallyEvaluated)
                        state, Loaded (pa.Id, errors)

        with e ->
            state, LoadFault(pa.Id, e)


    type AssemblyExporter = StatefulActor<Map<AssemblyId, int>, Assembly * bool * bool, PortableAssembly>
    type AssemblyLoader = StatefulActor<Map<string, LoadedAssemblyInfo>, PortableAssembly, AssemblyLoadResponse>

    let mkAssemblyExporter pickler (stateF : unit -> DynamicAssemblyCompilerState) : AssemblyExporter = 
        mkStatefulActor Map.empty (fun state (a,img,data) -> exportAssembly pickler (stateF()) state img data a)

    let mkAssemblyLoader pickler (serverId : Guid option) : AssemblyLoader =
        mkStatefulActor Map.empty (fun state pa -> loadAssembly pickler serverId state pa)

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