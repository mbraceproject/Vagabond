module internal Nessos.Vagabond.AssemblyManagement

open System
open System.IO
open System.Reflection

open Nessos.FsPickler

open Nessos.Vagabond
open Nessos.Vagabond.Utils
open Nessos.Vagabond.SliceCompilerTypes
open Nessos.Vagabond.AssemblyNaming
open Nessos.Vagabond.AssemblyCache
open Nessos.Vagabond.AssemblyManagementTypes
open Nessos.Vagabond.DataDependencyManagement

///
/// creates an exportable assembly package for given Assembly Id
///

let exportAssembly (state : VagabondState) (policy : AssemblyLoadPolicy) (id : AssemblyId) =
    // first, look up unmanaged assembly state
    match state.NativeAssemblyManager.TryFind id with
    | Some va -> state, va
    | None ->

    // check if slice of a local dynamic assembly
    match state.CompilerState.TryFindSliceInfo id.FullName with
    // is dynamic assembly slice which requires static initialization
    | Some (_, sliceInfo) when sliceInfo.RequiresStaticInitialization -> exportDataDependencies state sliceInfo
    | Some(_, sliceInfo) -> state, VagabondAssembly.CreateManaged(sliceInfo.Assembly, true, [||], [||])
    | None ->

    // look up export state
    match state.AssemblyExportState.TryFind id with
    | Some va -> state, va
    | None ->

    // assembly not a local dynamic assembly slice, need to lookup cache and AppDomain in that order; 
    // this is because cache contains vagabond metadata while AppDomain does not.
    match state.AssemblyCache.TryGetCachedAssembly id with
    | Some va -> state, va
    | None ->

    // finally, attempt to resolve from AppDomain
    let localAssembly =
        if id.CanBeResolvedLocally policy then tryLoadAssembly id.FullName
        else tryGetLoadedAssembly id.FullName

    match localAssembly with
    | Some a when policy.HasFlag AssemblyLoadPolicy.RequireIdentical && a.AssemblyId <> id ->
        let msg = sprintf "an incompatible version of '%s' has been loaded." id.FullName
        raise <| VagabondException(msg)

    | Some asmb -> 
        let va = VagabondAssembly.CreateManaged(asmb, false, [||], [||])
        { state with AssemblyExportState = state.AssemblyExportState.Add(va.Id, va) } , va

    | None ->
        let msg = sprintf "could not retrieve assembly '%s' from local environment." id.FullName
        raise <| VagabondException(msg)


///
/// assembly load status implementation
///

let getAssemblyLoadInfo (state : VagabondState) (policy : AssemblyLoadPolicy) (id : AssemblyId) =

    match state.AssemblyImportState.TryFind id with
    | Some loadState -> state, loadState
    // dynamic assembly slice generated in local process
    | None when state.CompilerState.IsLocalDynamicAssemblySlice id -> 
        let state, va = exportAssembly state policy id
        state, Loaded (id, true, va.Metadata)

    | None ->
        // look up assembly cache
        match state.AssemblyCache.TryGetCachedAssembly id with
        | Some va -> state, Loaded(id, false, va.Metadata)
        | None ->
            // Attempt resolving locally
            let localAssembly =
                if id.CanBeResolvedLocally policy then
                    tryLoadAssembly id.FullName
                else
                    tryGetLoadedAssembly id.FullName

            let info =
                match localAssembly with
                // if specified, check if loaded assembly has identical image hash
                | Some a when policy.HasFlag AssemblyLoadPolicy.RequireIdentical && a.AssemblyId <> id ->
                    let msg = sprintf "an incompatible version of '%s' has been loaded." id.FullName
                    LoadFault(id, VagabondException(msg))

                | Some a -> Loaded(id, true, { IsManagedAssembly = true ; IsDynamicAssemblySlice = false ; DataDependencies = [||] })
                | None -> NotLoaded id

            let state = { state with AssemblyImportState = state.AssemblyImportState.Add(id, info) }

            state, info


//
// assembly import protocol implementation
//

let loadAssembly (state : VagabondState) (policy : AssemblyLoadPolicy) (va : VagabondAssembly) =

    // update state with success
    let success state info = 
        let state = { state with AssemblyImportState = state.AssemblyImportState.Add(va.Id, info) }
        state, info

    let loadAssembly (va : VagabondAssembly) =
        let assembly = System.Reflection.Assembly.LoadFrom va.Image

        if assembly.FullName <> va.FullName then
            let msg = sprintf "Expected assembly '%s', but was '%s'." va.FullName assembly.FullName
            raise <| VagabondException(msg)

        elif policy.HasFlag AssemblyLoadPolicy.RequireIdentical && assembly.AssemblyId <> va.Id then
            let msg = sprintf "an incompatible version of '%s' has been loaded." va.FullName
            raise <| VagabondException(msg)
    try
        if va.Metadata.IsManagedAssembly then
            match state.AssemblyImportState.TryFind va.Id with
            // dynamic assembly slice generated in local process
            | None when state.CompilerState.IsLocalDynamicAssemblySlice va.Id -> 
                let state, va = exportAssembly state policy va.Id
                state, Loaded (va.Id, true, va.Metadata)
            // assembly not in state or loaded in AppDomain, attempt to load now
            | None 
            | Some (NotLoaded _) 
            | Some (LoadFault _)
            | Some (Loaded(_, false, _)) -> 
                loadAssembly va
                let state' = importDataDependencies state va
                success state' <| Loaded(va.Id, true, va.Metadata)
            // assembly already in AppDomain, update data dependencies if necessary
            | Some(Loaded(id, true,_)) ->
                let state' = importDataDependencies state va
                success state' <| Loaded(id, true, va.Metadata)
        else
            // add assembly to unmanaged dependency state
            let result = state.NativeAssemblyManager.Load va
            state, result

    with e -> state, LoadFault(va.Id, e)