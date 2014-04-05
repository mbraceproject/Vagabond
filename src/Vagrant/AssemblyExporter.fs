module internal Nessos.Vagrant.AssemblyExporter

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils

    type StaticInitializaterType = (FieldInfo * byte []) list * (FieldInfo * exn) list

    let exportAssembly (pickler : FsPickler) (state : DynamicAssemblyCompilerState) (generationIdx : Map<string, int>)
                        includeImage includeStaticInitializationData (assembly : Assembly) =

        let mkPortableAssembly dynamicAssemblyInfo =
            {
                FullName = assembly.FullName
                Image = if includeImage then Some <| File.ReadAllBytes assembly.Location else None
                DynamicAssemblyInfo = dynamicAssemblyInfo
            }
                 
        match state.TryFindSliceInfo assembly.FullName with
        | Some (dynAssembly, sliceInfo) ->

            let requiresStaticInitializers = sliceInfo.StaticFields.Length > 0
            let latestGeneration = generationIdx.TryFind assembly.FullName
            let isPartiallyEvaluated =
                dynAssembly.Profile.IsPartiallyEvaluatedSlice 
                    (fun t -> dynAssembly.TypeIndex.TryFind t.FullName |> Option.map (fun s -> s.Assembly)) 
                    sliceInfo.Assembly

            let generationIdx, generation, staticInitializer =
                if requiresStaticInitializers && includeStaticInitializationData then
                    // create pickled type initializers
                    let tryPickle (fI : FieldInfo) =
                        try
                            let value = fI.GetValue(null)
                            let pickle = pickler.Pickle<obj>(value)
                            Choice1Of2 (fI, pickle)
                        with e -> 
                            Choice2Of2 (fI, e)

                    let staticInitializers = sliceInfo.StaticFields |> Array.map tryPickle |> Seq.toList |> Choice.partition2
                    let data = pickler.Pickle<StaticInitializaterType>(staticInitializers)
                    let generation = 1 + (defaultArg latestGeneration -1)
                    let generationIdx = generationIdx.Add(assembly.FullName, generation)
                    generationIdx, Some generation, Some data
                else
                    generationIdx, latestGeneration, None

            let dynamicAssemblyInfo =
                {
                    SourceId = state.ServerId
                    DynamicAssemblyName = sliceInfo.DynamicAssemblyQualifiedName
                    SliceId = sliceInfo.SliceId

                    RequiresStaticInitializaters = requiresStaticInitializers
                    StaticInitializerGeneration = generation
                    IsPartiallyEvaluated = isPartiallyEvaluated
                    StaticInitializerData = staticInitializer
                }

            generationIdx, mkPortableAssembly (Some dynamicAssemblyInfo)

        | _ -> generationIdx, mkPortableAssembly None


    type LoadedAssemblyInfo =
        | UnLoaded
        | LoadedStatic
        | LoadedDynamic of requiresStaticInitialization:bool * generation:int option * isPartiallyEvaluated:bool

    let rec loadAssembly (pickler : FsPickler) (localId : Guid option) 
                        (state : Map<string, LoadedAssemblyInfo>) (pa : PortableAssembly) =

        let tryLoadAssembly (pa : PortableAssembly) =
            let loadedAssembly =
                System.AppDomain.CurrentDomain.GetAssemblies()
                |> Array.tryFind (fun a -> a.FullName = pa.FullName)

            let assembly =
                match loadedAssembly with
                | Some _ as a -> a
                | None -> 
                    // try load binary image
                    match pa.Image with
                    | None -> None
                    | Some bytes ->
                        let tmp = Path.GetTempFileName()
                        File.WriteAllBytes(tmp, bytes)
                        Some <| System.Reflection.Assembly.LoadFrom(tmp)

            assert (assembly |> Option.forall (fun a -> a.FullName = pa.FullName))
            assembly

        let loadStaticInitializer (data : byte []) =
            let tryLoad (fI : FieldInfo, bytes : byte []) =
                try 
                    let value = pickler.UnPickle<obj> bytes
                    fI.SetValue(null, value) ; None
                with e -> Some (fI, e)

            let pickles, serverErrors = pickler.UnPickle<StaticInitializaterType>(data)
            let localErrors = List.choose tryLoad pickles
            List.append serverErrors localErrors |> List.toArray
               
        let updateWith status = state.Add(pa.FullName, status)
        let currentStatus = defaultArg (state.TryFind pa.FullName) UnLoaded

        match localId, currentStatus, pa.DynamicAssemblyInfo with
        // static assembly already loaded in state
        | _, LoadedStatic, None -> state, Loaded (pa.FullName, [||])
        // static assembly not registered in state
        | _, UnLoaded, None ->
            match tryLoadAssembly pa with
            | None -> state, MissingAssemblyImage pa.FullName
            | Some a ->
                updateWith LoadedStatic, Loaded (pa.FullName, [||])

        // trying to load local dynamic assembly
        | Some localId, _, Some dynInfo when dynInfo.SourceId = localId -> state, Loaded(pa.FullName, [||])
        // dynamic assembly slice, not already loaded in state
        | _, UnLoaded, Some info ->
            match tryLoadAssembly pa with
            | None -> state, MissingAssemblyImage pa.FullName
            | Some a ->
                // assembly loaded in AppDomain, update state and recurse
                let state = updateWith <| LoadedDynamic(info.RequiresStaticInitializaters, None, info.IsPartiallyEvaluated)
                loadAssembly pickler localId state pa

        // handle impossible cases.
        | _, LoadedDynamic _, None
        | _, LoadedStatic, Some _ -> invalidOp "impossible state."

        // dynamic assemblies without static initializers
        | _, LoadedDynamic(requiresStaticInitialization = false), _ -> state, Loaded(pa.FullName, [||])
        // loaded dynamic assembly slices without any previous initialization data
        | _, LoadedDynamic(requiresStaticInitialization = true ; generation = None), Some info ->
            match info.StaticInitializerData with
            | None -> state, MissingStaticInitializer (pa.FullName, None)
            | Some data ->
                let errors = loadStaticInitializer data
                let state = updateWith <| LoadedDynamic(true, info.StaticInitializerGeneration, info.IsPartiallyEvaluated)
                state, Loaded(pa.FullName, errors)

        // load type initializers when previous initialization is present
        | _, LoadedDynamic(requiresStaticInitialization = true ; generation = Some previousGen ; isPartiallyEvaluated = isPartial), Some current ->
            match current.StaticInitializerGeneration with
            // silently discard updates if generation is older than currently loaded
            | Some gen when previousGen > gen -> state, Loaded (pa.FullName, [||])
            | _ ->
                match current.StaticInitializerData with
                // demand static initializer update if previous push was declared as partial
                | None when isPartial -> state, MissingStaticInitializer (pa.FullName, Some previousGen)
                | None -> state, Loaded (pa.FullName, [||])
                | Some data ->
                    let errors = loadStaticInitializer data
                    let state = updateWith <| LoadedDynamic(true, current.StaticInitializerGeneration, current.IsPartiallyEvaluated)
                    state, Loaded (pa.FullName, errors)


    type AssemblyExporter = StatefulAgent<Map<string,int>, Assembly * bool * bool, PortableAssembly>
    type AssemblyLoader = StatefulAgent<Map<string, LoadedAssemblyInfo>, PortableAssembly, AssemblyLoadResponse>

    let mkAssemblyExporter (pickler : FsPickler) (stateF : unit -> DynamicAssemblyCompilerState) : AssemblyExporter = 
        mkStatefulAgent Map.empty (fun state (a,img,data) -> exportAssembly pickler (stateF()) state img data a)


    let mkAssemblyLoader (pickler : FsPickler) (serverId : Guid option) : AssemblyLoader =
        mkStatefulAgent Map.empty (fun state pa -> loadAssembly pickler serverId state pa)



    // server-side protocol implementation

    let assemblySubmitProtocol (exporter : AssemblyExporter) (transportF : PortableAssembly list -> Async<AssemblyLoadResponse list>) (assemblies : Assembly list) =
        async {
            let index = assemblies |> Seq.map (fun a -> a.FullName, a) |> Map.ofSeq

            // Step 1. submit empty headers; no image data or type initializers.
            let headers = assemblies |> List.map (fun a -> exporter.PostAndReply(a, false, false))
            let! replies = transportF headers
        
            // Step 2. find missing image or type initializers and update request with actual data
            let updatePortableAssembly = 
                function
                | Loaded _ -> None
                | MissingAssemblyImage name -> Some <| exporter.PostAndReply(index.[name], true, true)
                | MissingStaticInitializer(name,_) -> Some <| exporter.PostAndReply(index.[name], false, true)

            let portableAssemblies = replies |> List.choose updatePortableAssembly
            let! replies = transportF portableAssemblies

            // Step 3. check results of second iteration; if client still replies negatively, fail
            if replies |> List.forall (function Loaded _ -> true | _ -> false) then
                // gather static initialization errors
                let errors = replies |> Seq.collect (function Loaded (_,errors) -> errors | _ -> [||]) |> Seq.toArray
                return errors
            else
                return invalidOp "Protocol error: could not publish requested assemblies."
        }