/// Vagabond assembly publish/receive protocol implementations
module Nessos.Vagabond.AssemblyProtocols

open System.Reflection

open Nessos.Vagabond.AssemblyNaming

/// Defines an abstract assembly load target; to be used by VagabondServer.
type IRemoteAssemblyReceiver =
    /// receives the assembly load state of the remote party for the given id's.
    abstract GetLoadedAssemblyInfo : AssemblyId [] -> Async<AssemblyLoadInfo []>
    /// upload a set of assembly packages to the remote party.
    abstract PushAssemblies : VagabondAssembly [] -> Async<AssemblyLoadInfo []>

/// Defines an abstract assembly exporter; to be used by VagabondClient.
type IRemoteAssemblyPublisher =
    /// receives a collection of dependencies required by remote publisher.
    abstract GetRequiredAssemblyInfo : unit -> Async<(AssemblyId * VagabondMetadata) []>
    /// request assembly packages from publisher.
    abstract PullAssemblies : AssemblyId [] -> Async<VagabondAssembly []>

type VagabondManager with
            
    /// <summary>
    ///     Apply a built-in assembly distribution protocol using a user-defined submit function.
    /// </summary>
    /// <param name="receiver">User provided assembly submit operation.</param>
    /// <param name="assemblies">Assemblies to be exported.</param>
    member v.SubmitDependencies(receiver : IRemoteAssemblyReceiver, dependencies : seq<VagabondAssembly>) = async {
        let dependencies = dependencies |> Seq.distinctBy (fun va -> va.Id) |> Seq.toArray

        // Step 1. submit assembly identifiers to receiver; get back loaded state
        let! info = receiver.GetLoadedAssemblyInfo (dependencies |> Array.map (fun va -> va.Id))
        
        // Step 2. detect dependencies that require uploading
        let tryGetMissingAssembly (info : AssemblyLoadInfo) =
            match info with
            | LoadFault(id, _)
            | NotLoaded id -> 
                Some <| v.GetVagabondAssembly id
            | Loaded(id,_,md) ->
                let va = v.GetVagabondAssembly id
                // detect if newer data dependencies exist
                let requireUpdate = (md.DataDependencies, va.Metadata.DataDependencies) ||> Array.exists2 (fun remote current -> remote.Generation < current.Generation)
                if requireUpdate then Some va
                else None

        let missingAssemblies = info |> Array.choose tryGetMissingAssembly
        let! loadResults = receiver.PushAssemblies missingAssemblies

        // Step 3. check load results; if client replies with fault, fail.
        let gatherErrors (info : AssemblyLoadInfo) =
            match info with
            | LoadFault(id, (:?VagabondException as e)) -> raise e
            | LoadFault(id, e) -> raise <| new VagabondException(sprintf "error on remote loading of assembly '%s'." id.FullName, e)
            | NotLoaded id -> raise <| new VagabondException(sprintf "could not load assembly '%s' on remote client." id.FullName)
            | Loaded (_, _, md) ->
                md.DataDependencies |> Array.filter(fun md -> match md.Data with Errored _ -> true | _ -> false) |> Some

        let dataErrors = loadResults |> Array.choose gatherErrors |> Array.concat
        return dataErrors
    }

    /// <summary>
    ///     Apply the built-in assembly distribution protocol using user-defined function.
    /// </summary>
    /// <param name="receiver">User provided assembly submit operation.</param>
    /// <param name="obj">Object, whose dependent assemblies are to be exported.</param>
    /// <param name="permitCompilation">Compile dynamic assemblies in the background, as required. Defaults to false.</param>
    /// <param name="includeNativeDependencies">Include declared native assemblies if specified. Defaults to true.</param>
    member v.SubmitObjectDependencies(receiver : IRemoteAssemblyReceiver, obj:obj, ?permitCompilation:bool, ?includeNativeDependencies:bool) = async {
        let dependencies = v.ComputeObjectDependencies(obj, ?permitCompilation = permitCompilation, ?includeNativeDependencies = includeNativeDependencies)
        return! v.SubmitDependencies(receiver, dependencies)
    }


    /// <summary>
    ///     Receive dependencies as supplied by the remote assembly publisher.
    /// </summary>
    /// <param name="publisher">The remote publisher</param>
    /// <param name="loadPolicy">Specifies local assembly resolution policy. Defaults to strong names only.</param>
    member v.ReceiveDependencies(publisher : IRemoteAssemblyPublisher, ?loadPolicy : AssemblyLookupPolicy) = async {

        // step 1. download dependencies required by publisher
        let! dependencies = publisher.GetRequiredAssemblyInfo()

        // step 2. resolve dependencies that are missing from client
        let tryCheckLoadStatus (id : AssemblyId, remoteMD : VagabondMetadata) =
            if v.IsLocalDynamicAssemblySlice id then None
            else
                match v.GetAssemblyLoadInfo(id, ?loadPolicy = loadPolicy) with
                | NotLoaded id
                | LoadFault (id,_) -> Some id
                // local state missing required metadata
                | Loaded(id, _, localMD) ->
                    // local state contains partial static initializers and incoming are of next generation
                    let requiresUpdate = (remoteMD.DataDependencies, localMD.DataDependencies) ||> Array.exists2 (fun r l -> l.Generation < r.Generation)
                    if requiresUpdate then Some id else None

        let missing = dependencies |> Array.choose tryCheckLoadStatus

        if missing.Length > 0 then
            // step 3. download missing dependencies
            let! assemblies = publisher.PullAssemblies missing
            let loadResults = v.LoadVagabondAssemblies(assemblies, ?loadPolicy = loadPolicy)

            let checkLoadResult (info : AssemblyLoadInfo) =
                match info with
                | NotLoaded id -> raise <| new VagabondException(sprintf "failed to load assembly '%s'" id.FullName)
                | LoadFault(_, (:? VagabondException as e)) -> raise e
                | LoadFault(id, e) -> raise <| new VagabondException(sprintf "failed to load assembly '%s'" id.FullName, e)
                | Loaded _ -> ()

            Array.iter checkLoadResult loadResults
    }