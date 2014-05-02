module internal Nessos.Vagrant.AssemblyLoader

    open System
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils


    //
    // assembly loader protocol implementation
    //

    type AssemblyLoader = StatefulActor<Map<AssemblyId, AssemblyInfo>, PortableAssembly, AssemblyLoadResponse>

    /// need an assembly resolution handler when loading assemblies at runtime

    let registerAssemblyResolutionHandler () = 
        System.AppDomain.CurrentDomain.add_AssemblyResolve <| 
            new ResolveEventHandler (fun _ args -> defaultArg (tryGetLoadedAssembly args.Name) null)

    /// portable assembly load protocol implementation

    let loadAssembly (pickler : FsPickler) (localId : Guid option) (state : Map<AssemblyId, AssemblyInfo>) (pa : PortableAssembly) =

        let updateWith info = state.Add(pa.Id, info)
        let raise e = state, LoadFault(pa.Id, e)

        let loadStaticInitializer (data : byte []) =
            let tryLoad (fI : FieldInfo, data : Exn<byte []>) =
                match data with
                | Success bytes ->
                    try
                        let value = pickler.UnPickle<obj> bytes
                        fI.SetValue(null, value) ; None
                    with e -> Some(fI, e)
                | Error e -> Some(fI, e)

            let initializers = pickler.UnPickle<AssemblyExporter.StaticInitializers>(data)
            Array.choose tryLoad initializers

        // load assembly image
        let loadAssembly (pa : PortableAssembly) =

            match tryLoadAssembly pa.FullName with
            | Some a when a.AssemblyId = pa.Id ->
                let info = { pa.Info with IsImageLoaded = true }
                updateWith info, LoadSuccess(pa.Id, [||])

            | Some _ -> raise <| new VagrantException(sprintf "an incompatible version of '%s' has been loaded in the client." pa.FullName)
            | None ->
                // try load binary image
                match pa.Image with
                | None -> raise <| new VagrantException(sprintf "portable assembly '%s' missing binary image." pa.FullName)
                | Some bytes ->
                    let assembly =
                        match pa.Symbols with
                        | None -> System.Reflection.Assembly.Load(bytes)
                        | Some symbols -> System.Reflection.Assembly.Load(bytes, symbols)

                    if assembly.FullName <> pa.FullName then
                        let msg = sprintf "Expected assembly '%s', received '%s'." pa.FullName assembly.FullName
                        raise <| new VagrantException(msg)
                    else
                        let staticInitializerErrors =
                            match pa.StaticInitializer with
                            | None -> [||]
                            | Some (_,data) -> loadStaticInitializer data

                        let info = { pa.Info with IsImageLoaded = true }

                        updateWith info, LoadSuccess(pa.Id, staticInitializerErrors)

        try
            match localId, state.TryFind pa.Id, pa.Info.DynamicAssemblySliceInfo with
            // dynamic assembly slice generated in local process
            | Some id, _, Some dyn when dyn.SourceId = id -> state, LoadSuccess (pa.Id, [||])
            // assembly not registered in state, attempt to load now
            | _, None, _ -> loadAssembly pa
            // assembly registered in state, update static initializers if required
            | _, Some storedInfo, _ ->
                let currentGen =
                    match storedInfo.DynamicAssemblySliceInfo with
                    | None -> -1
                    | Some info -> info.StaticInitializerGeneration

                match pa.StaticInitializer with
                | Some(gen,data) when gen > currentGen ->
                    let staticInitializerErrors = loadStaticInitializer data
                    updateWith pa.Info, LoadSuccess(pa.Id, staticInitializerErrors)
                // silently discard update if generation older than previously recorded
                | _ -> state, LoadSuccess(pa.Id, [||])

        with e -> state, LoadFault(pa.Id, e)
    
    let mkAssemblyLoader pickler (serverId : Guid option) : AssemblyLoader =
        mkStatefulActor Map.empty (fun state pa -> loadAssembly pickler serverId state pa)

    let getAssemblyLoadState (loader : AssemblyLoader) (id : AssemblyId) =

        let defaultInfo = 
            {
                Id = id
                IsImageLoaded = false
                IsSymbolsLoaded = false
                DynamicAssemblySliceInfo = None
            }

        let pa = { Info = defaultInfo ; Image = None ; Symbols = None ; StaticInitializer = None }

        // force load assembly in state if present in appdomain/gac
        let _ = loader.PostAndReply pa

        match loader.CurrentState.TryFind id with
        | Some info -> info
        | None -> defaultInfo