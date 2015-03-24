/// Vagabond assembly publish/receive protocol implementations
module Nessos.Vagabond.AssemblyProtocols

open System.Reflection

open Nessos.Vagabond.AssemblyNaming

/// Defines an abstract assembly load target; to be used by VagabondServer.
type IRemoteAssemblyReceiver =
    /// receives the assembly load state of the remote party for the given id's.
    abstract GetLoadedAssemblyInfo : AssemblyId list -> Async<AssemblyLoadInfo list>
    /// upload a set of assembly packages to the remote party.
    abstract PushAssemblies : VagabondAssembly list -> Async<AssemblyLoadInfo list>

/// Defines an abstract assembly exporter; to be used by VagabondClient.
type IRemoteAssemblyPublisher =
    /// receives a collection of dependencies required by remote publisher.
    abstract GetRequiredAssemblyInfo : unit -> Async<(AssemblyId * VagabondMetadata option) list>
    /// request assembly packages from publisher.
    abstract PullAssemblies : AssemblyId list -> Async<VagabondAssembly list>

type VagabondManager with
            
    /// <summary>
    ///     Apply a built-in assembly distribution protocol using a user-defined submit function.
    /// </summary>
    /// <param name="receiver">User provided assembly submit operation.</param>
    /// <param name="assemblies">Assemblies to be exported.</param>
    member v.SubmitAssemblies(receiver : IRemoteAssemblyReceiver, dependencies : seq<AssemblyId>) = async {
        let dependencies = Seq.toList dependencies

        // Step 1. submit assembly identifiers to receiver; get back loaded state
        let! info = receiver.GetLoadedAssemblyInfo dependencies
        
        // Step 2. detect dependencies that require posting
        let tryGetAssemblyPackage (info : AssemblyLoadInfo) =
            match info with
            | LoadFault(id, (:?VagabondException as e)) -> raise e
            | LoadFault(id, e) -> 
                raise <| new VagabondException(sprintf "error on remote loading of assembly '%s'." id.FullName, e)
            | NotLoaded id -> 
                Some <| v.GetVagabondAssembly(id)
            | Loaded(id,_,Some si) when si.IsPartial ->
                Some <| v.GetVagabondAssembly(id)
            | Loaded _ -> None

        let assemblyPackages = info |> List.choose tryGetAssemblyPackage
        let! loadResults = receiver.PushAssemblies assemblyPackages

        // Step 3. check load results; if client replies with fault, fail.
        let gatherErrors (info : AssemblyLoadInfo) =
            match info with
            | LoadFault(id, (:?VagabondException as e)) -> raise e
            | LoadFault(id, e) -> raise <| new VagabondException(sprintf "error on remote loading of assembly '%s'." id.FullName, e)
            | NotLoaded id -> raise <| new VagabondException(sprintf "could not load assembly '%s' on remote client." id.FullName)
            | Loaded(_,_,Some info) -> Some info.ErroredFields
            | Loaded _ -> None

        let staticInitializationErrors = loadResults |> List.choose gatherErrors |> Array.concat
        return staticInitializationErrors
    }

    /// <summary>
    ///     Apply the built-in assembly distribution protocol using user-defined function.
    /// </summary>
    /// <param name="receiver">User provided assembly submit operation.</param>
    /// <param name="obj">Object, whose dependent assemblies are to be exported.</param>
    /// <param name="permitCompilation">Compile dynamic assemblies in the background, as required. Defaults to false.</param>
    /// <param name="includeNativeAssemblies">Include declared native assemblies if specified. Defaults to true.</param>
    member v.SubmitObjectDependencies(receiver : IRemoteAssemblyReceiver, obj:obj, ?permitCompilation:bool, ?includeNativeAssemblies:bool) = async {
        let includeNativeAssemblies = defaultArg includeNativeAssemblies true
        let managedDependencies = v.ComputeObjectDependencies(obj, ?permitCompilation = permitCompilation) |> List.map (fun a -> a.AssemblyId)
        let dependencies = 
            if includeNativeAssemblies then 
                let unmanaged = v.UnManagedDependencies |> List.map (fun um -> um.Id)
                List.append unmanaged managedDependencies
            else managedDependencies

        return! v.SubmitAssemblies(receiver, dependencies)
    }


    /// <summary>
    ///     Receive dependencies as supplied by the remote assembly publisher.
    /// </summary>
    /// <param name="publisher">The remote publisher</param>
    /// <param name="loadPolicy">Specifies local assembly resolution policy. Defaults to strong names only.</param>
    member v.ReceiveDependencies(publisher : IRemoteAssemblyPublisher, ?loadPolicy : AssemblyLoadPolicy) = async {

        // step 1. download dependencies required by publisher
        let! dependencies = publisher.GetRequiredAssemblyInfo()

        // step 2. resolve dependencies that are missing from client
        let tryCheckLoadStatus (id : AssemblyId, metadata : VagabondMetadata option) =
            if v.IsLocalDynamicAssemblySlice id then None
            else
                match v.GetAssemblyLoadInfo(id, ?loadPolicy = loadPolicy), metadata with
                | NotLoaded id, _
                | LoadFault (id,_), _ -> Some id
                // local state missing required metadata
                | Loaded(id, _, None), Some _ -> Some id
                // local state contains partial stat initializers and incoming is of next generation
                | Loaded(id, _, Some md), Some md' when md.IsPartial && md.Generation < md'.Generation -> Some id
                // local state contains dependency, filter out
                | Loaded _, _ -> None

        let missing = dependencies |> List.choose tryCheckLoadStatus

        if missing.Length > 0 then
            // step 3. download assembly packages for missing dependencies
            let! assemblies = publisher.PullAssemblies missing
            let loadResults = v.LoadVagabondAssemblies(assemblies, ?loadPolicy = loadPolicy)

            let checkLoadResult (info : AssemblyLoadInfo) =
                match info with
                | NotLoaded id -> raise <| new VagabondException(sprintf "failed to load assembly '%s'" id.FullName)
                | LoadFault(_, (:? VagabondException as e)) -> raise e
                | LoadFault(id, e) -> raise <| new VagabondException(sprintf "failed to load assembly '%s'" id.FullName, e)
                | Loaded _ -> ()

            List.iter checkLoadResult loadResults
    }