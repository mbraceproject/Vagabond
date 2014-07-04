module internal Nessos.Vagrant.AssemblyManagement

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.SliceCompilerTypes
    open Nessos.Vagrant.AssemblyCache

    type StaticInitializers = (FieldInfo * Exn<byte []>) []

    type VagrantState =
        {
            CompilerState : DynamicAssemblyCompilerState
            AssemblyExportState : Map<AssemblyId, int>
            AssemblyImportState : Map<AssemblyId, AssemblyLoadInfo>

            Pickler : BasePickler
            AssemblyCache : AssemblyCache
        }

    ///
    /// exports a portable assembly
    ///

    let exportAssembly (state : VagrantState) (policy : AssemblyLoadPolicy)
                        (includeImage : bool) (id : AssemblyId) =

        match state.CompilerState.TryFindSliceInfo id.FullName with
        | Some (dynAssembly, sliceInfo) when sliceInfo.RequiresStaticInitialization ->

            let generation = 1 + defaultArg (state.AssemblyExportState.TryFind id) -1
                
            let tryPickle (fI : FieldInfo) =
                try
                    let value = fI.GetValue(null)
                    let pickle = state.Pickler.Pickle<obj>(value)
                    fI, Success pickle
                with e -> 
                    fI, Error e

            let staticInitializers = Array.map tryPickle sliceInfo.StaticFields
            let data = state.Pickler.Pickle<StaticInitializers>(staticInitializers)

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

            let generationIndex = state.AssemblyExportState.Add(id, generation)
            let pa = state.AssemblyCache.CreatePortableAssembly(sliceInfo.Assembly, includeImage = includeImage)

            { state with AssemblyExportState = generationIndex }, 
                { pa with StaticInitializer = Some staticInitializer }

        | Some(_, sliceInfo) -> 
            let pa = state.AssemblyCache.CreatePortableAssembly(sliceInfo.Assembly, includeImage = includeImage)
            state, pa

        | None ->
            // assembly not a local dynamic assembly slice, need to lookup cache and AppDomain
            // in that order; this is because cache contains vagrant metadata while AppDomain does not.
            match state.AssemblyCache.TryGetCachedAssemblyInfo id with
            | Some info -> 
                let pa = state.AssemblyCache.CreatePortableAssembly(info, includeImage = includeImage)
                state, pa

            | None ->

                // finally, attempt to resolve from AppDomain

                let localAssembly =
                    if id.CanBeResolvedLocally policy then tryLoadAssembly id.FullName
                    else tryGetLoadedAssembly id.FullName

                match localAssembly with
                | Some a when policy.HasFlag AssemblyLoadPolicy.RequireIdentical && a.AssemblyId <> id ->
                    let msg = sprintf "an incompatible version of '%s' has been loaded." id.FullName
                    raise <| VagrantException(msg)

                | Some a -> 
                    let pa = state.AssemblyCache.CreatePortableAssembly(a, includeImage = includeImage)
                    state, pa

                | None ->
                    let msg = sprintf "could not retrieve assembly '%s' from local environment." id.FullName
                    raise <| VagrantException(msg)


    //
    // assembly import protocol implementation
    //

    let importAssembly (state : VagrantState) (policy : AssemblyLoadPolicy) (pa : PortableAssembly) =

        // update state with success
        let success info = 
            let state = { state with AssemblyImportState = state.AssemblyImportState.Add(pa.Id, info) }
            state, info

        let loadInAppDomain = not <| policy.HasFlag AssemblyLoadPolicy.CacheOnly

        // loads the static initializer for given portable assembly
        // requires the assembly to be already loaded in the current AppDomain
        let tryLoadStaticInitializer (previous : StaticInitializationInfo option) (cacheInfo : CachedAssemblyInfo) =
            let tryLoad (fI : FieldInfo, data : Exn<byte []>) =
                match data with
                | Success bytes ->
                    try
                        let value = state.Pickler.UnPickle<obj> bytes
                        fI.SetValue(null, value) ; None
                    with e -> Some(fI, e)
                | Error e -> Some(fI, e)

            match previous, cacheInfo.StaticInitializer with
            | None, None -> Loaded (cacheInfo.Id, true, None)
            // keep the previous static initializer if PA has none
            | Some previous, None -> Loaded(cacheInfo.Id, true, Some previous)
            // silently discard if loaded generation larger than current
            | Some info, Some (_,init) when info.Generation > init.Generation ->  Loaded(cacheInfo.Id, true, Some info)

            // perform the static initialization
            | _, Some (path, init) ->
                let data = File.ReadAllBytes path
                let initializers = state.Pickler.UnPickle<StaticInitializers>(data)
                let errors = Array.choose tryLoad initializers
                let info = { Generation = init.Generation ; Errors = errors ; IsPartial = init.IsPartial }
                Loaded(cacheInfo.Id, true, Some info)


        let loadAssembly (pa : PortableAssembly) =

            // Attempt resolving locally
            let localAssembly =
                if pa.Id.CanBeResolvedLocally policy then
                    tryLoadAssembly pa.FullName
                else
                    tryGetLoadedAssembly pa.FullName

            match localAssembly with
            // if specified, check if loaded assembly has identical image hash
            | Some a when policy.HasFlag AssemblyLoadPolicy.RequireIdentical && a.AssemblyId <> pa.Id ->
                let msg = sprintf "an incompatible version of '%s' has been loaded." pa.FullName
                raise <| VagrantException(msg)

            // if GAC, do not cache, just report as loaded
            | Some a when a.GlobalAssemblyCache -> success <| Loaded (pa.Id, true, None)
            
            // local assemblies not in GAC are to be cached
            | Some a ->
                let cacheInfo =
                    if pa.Image.IsSome then
                        state.AssemblyCache.Cache pa
                    else
                        state.AssemblyCache.Cache(a, asId = pa.Id)
                    
                success <| Loaded(pa.Id, true, cacheInfo.StaticInitializer |> Option.map snd)

            | None when pa.Image.IsSome || state.AssemblyCache.IsCachedAssembly pa.Id ->
                // cache assembly and load from cache location
                let cacheInfo = state.AssemblyCache.Cache pa
                if loadInAppDomain then
                    let assembly = System.Reflection.Assembly.LoadFrom cacheInfo.Location

                    if assembly.FullName <> pa.FullName then
                        let msg = sprintf "Expected assembly '%s', received '%s'." pa.FullName assembly.FullName
                        raise <| VagrantException(msg)

                    elif policy.HasFlag AssemblyLoadPolicy.RequireIdentical && assembly.AssemblyId <> pa.Id then
                        let msg = sprintf "an incompatible version of '%s' has been loaded." pa.FullName
                        raise <| VagrantException(msg)

                    else
                        success <| tryLoadStaticInitializer None cacheInfo
                else
                    success <| Loaded(pa.Id, false, cacheInfo.StaticInitializer |> Option.map snd)
                
            | None -> state, NotLoaded pa.Id

        try
            match state.AssemblyImportState.TryFind pa.Id with
            // dynamic assembly slice generated in local process
            | None when state.CompilerState.IsLocalDynamicAssemblySlice pa.Id -> success <| Loaded (pa.Id, true, None)
            // assembly not registered in state, attempt to load now
            | None -> loadAssembly pa
            // assembly loaded with static initializers, attempt to update
            | Some (Loaded(id, isLoadedLocal, Some info) as l) ->
                if not isLoadedLocal && loadInAppDomain then loadAssembly pa
                else
                    let cacheInfo = state.AssemblyCache.Cache pa
                        
                    if loadInAppDomain || isLoadedLocal then
                        success <| tryLoadStaticInitializer (Some info) cacheInfo
                    else
                        success <| Loaded(id, false, cacheInfo.StaticInitializer |> Option.map snd)

            | Some result -> state, result

        with e -> state, LoadFault(pa.Id, e)