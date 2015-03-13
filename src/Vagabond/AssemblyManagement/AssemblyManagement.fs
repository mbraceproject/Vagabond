module internal Nessos.Vagabond.AssemblyManagement

open System
open System.IO
open System.Reflection

open Nessos.FsPickler

open Nessos.Vagabond
open Nessos.Vagabond.Utils
open Nessos.Vagabond.SliceCompilerTypes
open Nessos.Vagabond.AssemblyCache

/// Immutable Vagabond state object
type VagabondState =
    {
        /// Dynamic assembly compiler state
        CompilerState : DynamicAssemblyCompilerState
        /// Locally compiled dynamic assembly export states
        AssemblyExportState : Map<AssemblyId, VagabondMetadata>
        /// Local assembly import state
        AssemblyImportState : Map<AssemblyId, AssemblyLoadInfo>
        /// Vagabond Serializer instance
        Serializer : FsPicklerSerializer
        /// Assembly Cache instance
        AssemblyCache : AssemblyCache
    }

/// registers an assembly resolution handler based on AppDomain lookups;
/// this is needed since assembly lookups often fail when loaded at runtime.
let registerAssemblyResolutionHandler () = 
    System.AppDomain.CurrentDomain.add_AssemblyResolve <|
        new ResolveEventHandler (fun _ args -> defaultArg (tryGetLoadedAssembly args.Name) null)

///
/// creates an exportable assembly package for given Assembly Id
///

let exportAssembly (state : VagabondState) (policy : AssemblyLoadPolicy) (id : AssemblyId) =

    match state.CompilerState.TryFindSliceInfo id.FullName with
    // is dynamic assembly slice which requires static initialization
    | Some (dynAssembly, sliceInfo) when sliceInfo.RequiresStaticInitialization ->

        let generation =
            match state.AssemblyExportState.TryFind id with
            | None -> 0
            | Some md -> md.Generation + 1
                
        let tryPickle (fI : FieldInfo) =
            try
                let value = fI.GetValue(null)
                let size = state.Serializer.ComputeSize value
                Choice1Of2 (fI, value)
            with e -> 
                Choice2Of2 (fI, e)

        let initializers, errors = Array.map tryPickle sliceInfo.StaticFields |> Choice.split

        let isPartiallyEvaluated = 
            dynAssembly.Profile.IsPartiallyEvaluatedSlice
                (dynAssembly.TryGetSlice >> Option.map (fun s -> s.Assembly)) 
                    sliceInfo.Assembly

        let metadata =
            {
                Generation = generation
                IsPartial = isPartiallyEvaluated
                PickledFields = initializers |> Array.map (fun (f,_) -> f.ToString(), state.Serializer.PickleTyped f)
                ErroredFields = errors |> Array.map (function (f,_) as err -> f.ToString(), state.Serializer.PickleTyped err)
            }

        let pkg = state.AssemblyCache.WriteStaticInitializers(sliceInfo.Assembly, initializers, metadata)
        let generationIndex = state.AssemblyExportState.Add(id, metadata)

        { state with AssemblyExportState = generationIndex }, pkg

    | Some(_, sliceInfo) -> 
        let va = state.AssemblyCache.CreateVagabondAssembly(sliceInfo.Assembly)
        state, va

    | None ->
        // assembly not a local dynamic assembly slice, need to lookup cache and AppDomain in that order; 
        // this is because cache contains vagabond metadata while AppDomain does not.
        match state.AssemblyCache.TryGetCachedAssemblyInfo id with
        | Some pkg -> state, pkg
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
                let pkg = state.AssemblyCache.CreateVagabondAssembly asmb
                state, pkg

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
    | None when state.CompilerState.IsLocalDynamicAssemblySlice id -> state, Loaded (id, true, None)
    | None ->
        // look up assembly cache
        match state.AssemblyCache.TryGetCachedAssemblyInfo id with
        | Some va -> state, Loaded(id, false, va.Metadata |> Option.map fst)
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

                | Some a -> Loaded(id, true, None)
                | None -> NotLoaded id

            let state = { state with AssemblyImportState = state.AssemblyImportState.Add(id, info) }

            state, info


//
// assembly import protocol implementation
//

let loadAssembly (state : VagabondState) (policy : AssemblyLoadPolicy) (va : VagabondAssembly) =

    // update state with success
    let success info = 
        let state = { state with AssemblyImportState = state.AssemblyImportState.Add(va.Id, info) }
        state, info

    // loads the static initializers for given assembly package
    // requires the assembly to be already loaded in the current AppDomain
    let tryLoadStaticInitializers (previous : VagabondMetadata option) (pkg : VagabondAssembly) =
        match previous, pkg.Metadata with
        | None, None -> Loaded (pkg.Id, true, None)
        // keep the previous static initializer if PA has none
        | Some _, None -> Loaded(pkg.Id, true, previous)
        // silently discard if loaded generation larger than current
        | Some info, Some (md,_) when info.Generation > md.Generation -> Loaded(pkg.Id, true, Some info)

        // perform the static initialization
        | _, Some (md, init) ->
            let initializers = state.AssemblyCache.ReadStaticInitializers init
            for fI, value in initializers do fI.SetValue(null, value)
            Loaded(pkg.Id, true, Some md)

    let loadAssembly (va : VagabondAssembly) =
        let assembly = System.Reflection.Assembly.LoadFrom va.Image

        if assembly.FullName <> va.FullName then
            let msg = sprintf "Expected assembly '%s', but was '%s'." va.FullName assembly.FullName
            raise <| VagabondException(msg)

        elif policy.HasFlag AssemblyLoadPolicy.RequireIdentical && assembly.AssemblyId <> va.Id then
            let msg = sprintf "an incompatible version of '%s' has been loaded." va.FullName
            raise <| VagabondException(msg)

        else
            success <| tryLoadStaticInitializers None va

    try
        match state.AssemblyImportState.TryFind va.Id with
        // dynamic assembly slice generated in local process
        | None when state.CompilerState.IsLocalDynamicAssemblySlice va.Id -> success <| Loaded (va.Id, true, None)
        // assembly not registered in state, attempt to load now
        | None 
        | Some (NotLoaded _) 
        | Some (LoadFault _) -> loadAssembly va
        | Some (Loaded(id, true, Some info)) -> success <| tryLoadStaticInitializers (Some info) va
        | Some (Loaded _ as result) -> state, result

    with e -> state, LoadFault(va.Id, e)