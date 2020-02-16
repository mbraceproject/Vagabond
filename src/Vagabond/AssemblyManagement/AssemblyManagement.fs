module internal MBrace.Vagabond.AssemblyManagement

open System
open System.IO
open System.Reflection

open MBrace.FsPickler

open MBrace.Vagabond
open MBrace.Vagabond.Utils
open MBrace.Vagabond.SliceCompilerTypes
open MBrace.Vagabond.AssemblyNaming
open MBrace.Vagabond.AssemblyCache
open MBrace.Vagabond.AssemblyManagementTypes
open MBrace.Vagabond.DataDependencyManagement

///
/// creates an exportable assembly package for given Assembly Id
///

let tryExportAssembly (state : VagabondState) (policy : AssemblyLookupPolicy) (id : AssemblyId) =
    // first, look up unmanaged assembly state
    match state.NativeAssemblyManager.TryFind id with
    | Some va -> state, Some va
    | None ->

    // check if slice of a local dynamic assembly
    match state.CompilerState.TryFindSliceInfo id.FullName with
    // is local dynamic assembly slice, handle separately
    | Some (_, sliceInfo) -> 
        let state, va = exportDataDependencies state sliceInfo
        state, Some va

    | None ->

    // check if slice of compiled in-memory assembly
    match state.CompilerState.InMemoryAssemblies.TryFind id.FullName with
    | Some info ->
        let va = VagabondAssembly.FromManagedAssembly(info.CompiledAssembly, false, [||], [||])
        { state with AssemblyLoadState = state.AssemblyLoadState.Add(va.Id, LoadedAssembly va) }, Some va

    | None ->

    match state.AssemblyLoadState.TryFind id with
    | Some (ExportedSlice _) -> invalidOp <| sprintf "internal error: invalid vagabond state for assembly '%s'." id.FullName
    | Some (LoadedAssembly va | ImportedAssembly va | ImportedSlice va) -> state, Some va
    | None ->
        // attempt resolving from AppDomain
        let localAssembly =
            if id.CanBeResolvedLocally policy then tryLoadAssembly id.FullName
            else tryGetLoadedAssembly id.FullName

        match localAssembly with
        | Some a when policy.HasFlag AssemblyLookupPolicy.RuntimeResolutionRequireIdenticalHash && a.AssemblyId <> id ->
            let msg = sprintf "an incompatible version of '%s' has been loaded." id.FullName
            raise <| new VagabondException(msg)

        | Some asmb -> 
            let va = VagabondAssembly.FromManagedAssembly(asmb, false, [||], [||])
            { state with AssemblyLoadState = state.AssemblyLoadState.Add(va.Id, LoadedAssembly va) }, Some va

        | None when policy.HasFlag AssemblyLookupPolicy.ResolveVagabondCache ->
            let va = state.AssemblyCache.TryGetCachedAssembly id
            state, va

        | None -> state, None

///
/// assembly load status implementation
///

let getAssemblyLoadInfo (state : VagabondState) (policy : AssemblyLookupPolicy) (id : AssemblyId) =

    // dynamic assembly slice generated in local process
    if state.CompilerState.IsLocalDynamicAssemblySlice id then
        let state, va = tryExportAssembly state policy id
        state, Loaded (id, true, Option.get(va).Metadata)
    else
        match state.AssemblyLoadState.TryFind id with
        | Some (ExportedSlice _) -> invalidOp <| sprintf "internal error: invalid vagabond state for assembly '%s'." id.FullName
        | Some (ImportedAssembly va | ImportedSlice va | LoadedAssembly va) -> state, Loaded(id, true, va.Metadata)
        | None ->
            // attempt resolving from AppDomain
            let localAssembly =
                if id.CanBeResolvedLocally policy then tryLoadAssembly id.FullName
                else tryGetLoadedAssembly id.FullName 

            match localAssembly with
            // if specified, check if loaded assembly has identical image hash
            | Some a when policy.HasFlag AssemblyLookupPolicy.RuntimeResolutionRequireIdenticalHash && a.AssemblyId <> id ->
                let msg = sprintf "an incompatible version of '%s' has been loaded." id.FullName
                state, LoadFault(id, VagabondException(msg))

            | Some a ->
                let va = VagabondAssembly.FromManagedAssembly(a, false, [||], [||])
                let state = { state with AssemblyLoadState = state.AssemblyLoadState.Add(va.Id, LoadedAssembly va) }
                state, Loaded(id, true, va.Metadata)

            | None when policy.HasFlag AssemblyLookupPolicy.ResolveVagabondCache ->
                match state.AssemblyCache.TryGetCachedAssembly id with
                | None -> state, NotLoaded id
                | Some va -> state, Loaded(id, false, va.Metadata)

            | None -> state, NotLoaded id


let getAssemblyLoadInfos (state : VagabondState) (policy : AssemblyLookupPolicy) (ids : seq<AssemblyId>) =
    let mutable state = state
    let results = new ResizeArray<AssemblyLoadInfo> ()
    for id in ids do
        let state', li = getAssemblyLoadInfo state policy id
        state <- state'
        results.Add li
    state, results.ToArray()


//
// assembly import protocol implementation
//

let loadAssembly (state : VagabondState) (policy : AssemblyLookupPolicy) (va : VagabondAssembly) =
    // load assembly to the current AppDomain
    let loadAssembly (va : VagabondAssembly) =
        // copy potential mixed mode assembly to native assembly folder
        if va.Metadata.ProcessorArchitecture <> ProcessorArchitecture.MSIL then
            match state.NativeAssemblyManager.Load va with
            | LoadFault(_,e) -> raise e
            | _ -> ()

        let assembly = Utils.currentLoadContext.LoadFromAssemblyPath va.Image

        if assembly.FullName <> va.FullName then
            let msg = sprintf "Expected assembly '%s', but was '%s'." va.FullName assembly.FullName
            raise <| VagabondException(msg)

        elif policy.HasFlag AssemblyLookupPolicy.RuntimeResolutionRequireIdenticalHash && assembly.AssemblyId <> va.Id then
            let msg = sprintf "an incompatible version of '%s' has been loaded." va.FullName
            raise <| VagabondException(msg)

    try
        if not va.Metadata.IsNativeAssembly then
            // dynamic assembly slice generated in local process
            if state.CompilerState.IsLocalDynamicAssemblySlice va.Id then
                let state, va = tryExportAssembly state policy va.Id
                let va = Option.get va
                state, Loaded (va.Id, true, va.Metadata)
            else
                match state.AssemblyLoadState.TryFind va.Id with
                | Some (ExportedSlice _) -> invalidOp <| sprintf "internal error: invalid vagabond state for assembly '%s'." va.Id.FullName
                // loaded static assembly, return as-is
                | Some (LoadedAssembly va | ImportedAssembly va) -> state, Loaded(va.Id, true, va.Metadata)
                // loaded dynamic assembly slice, update data dependencies as necessary
                | Some (ImportedSlice curr) ->
                    let state = importDataDependencies state (Some curr) va
                    state, Loaded(va.Id, true, va.Metadata)
                | None ->
                    // attempt resolving from AppDomain first
                    let localAssembly =
                        if va.Id.CanBeResolvedLocally policy then tryLoadAssembly va.Id.FullName
                        else tryGetLoadedAssembly va.Id.FullName 

                    match localAssembly with
                    // if specified, check if loaded assembly has identical image hash
                    | Some a when policy.HasFlag AssemblyLookupPolicy.RuntimeResolutionRequireIdenticalHash && a.AssemblyId <> va.Id ->
                        let msg = sprintf "an incompatible version of '%s' has been loaded." va.Id.FullName
                        state, LoadFault(va.Id, VagabondException(msg))

                    | Some a ->
                        let va = VagabondAssembly.FromManagedAssembly(a, false, [||], [||])
                        let state = { state with AssemblyLoadState = state.AssemblyLoadState.Add(va.Id, LoadedAssembly va) }
                        state, Loaded(va.Id, true, va.Metadata)

                    | None ->
                        // proceed with loading Vagabond assembly
                        do loadAssembly va
                        let state =
                            if va.Metadata.IsDynamicAssemblySlice then
                                importDataDependencies state None va
                            else
                                { state with AssemblyLoadState = state.AssemblyLoadState.Add(va.Id, ImportedAssembly va) }

                        state, Loaded(va.Id, true, va.Metadata)
        else
            // add assembly to unmanaged dependency state
            let result = state.NativeAssemblyManager.Load va
            state, result

    with e -> state, LoadFault(va.Id, e)