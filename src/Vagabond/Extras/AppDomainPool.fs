namespace Nessos.Vagabond.AppDomainPool

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization
open System.Threading
open System.Threading.Tasks

open Microsoft.FSharp.Control

open Nessos.FsPickler
open Nessos.Vagabond

/// Interface implemented by MarshalByRef types
/// used for managing Application domains.
/// Types implementing the interface must carry
/// a parameterless constructor.
type IAppDomainManager =
    /// AppDomain initializer method.
    abstract Initialize : unit -> unit
    /// AppDomain finalization method.
    abstract Finalize : unit -> unit
    /// Gets the number of tasks currently active in the AppDomain. 
    /// AppDomains with non-zero task count will never be unloaded
    /// by the AppDomainPool.
    abstract TaskCount : int
    /// Gets the last time this app domain instance was used.
    /// Used by the AppDomainPool to unload unused instances.
    abstract LastUsed : DateTime

/// Marshalled task completion source for async computation over AppDomains
type MarshalledTaskCompletionSource<'T> () =
    inherit MarshalByRefObject ()
    let tcs = new TaskCompletionSource<'T> ()
    /// Set a value as task result
    member __.SetResult(t : 'T) = tcs.SetResult t
    /// Set an exception as task result
    member __.SetException(e : exn) = tcs.SetException e
    /// Cancels the task
    member __.SetCanceled() = tcs.SetCanceled()
    /// Gets the local task instance.
    member __.Task = tcs.Task

/// Marshalled cancellation token source for use over AppDomains
type MarshalledCancellationTokenSource () =
    inherit MarshalByRefObject ()
    let cts = new CancellationTokenSource()
    /// Cancel the cancellation token source
    member __.Cancel() = cts.Cancel()
    /// Gets the local cancellation token
    member __.CancellationToken = cts.Token

[<AutoOpen>]
module private Impl =

    /// initialize a new AppDomain with given friendly name
    let initAppDomain (name : string) =
        let currentDomain = AppDomain.CurrentDomain
        let appDomainSetup = currentDomain.SetupInformation
        let evidence = new Security.Policy.Evidence(currentDomain.Evidence)
        AppDomain.CreateDomain(name, evidence, appDomainSetup)

    /// initialize an object in the target Application domain
    let initAppDomainObject<'T when 'T :> MarshalByRefObject 
                                and 'T : (new : unit -> 'T)> (targetDomain : AppDomain) =

        let assemblyName = typeof<'T>.Assembly.FullName
        let typeName = typeof<'T>.FullName
        let culture = System.Globalization.CultureInfo.CurrentCulture
        let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance
        let handle = targetDomain.CreateInstance(assemblyName, typeName, false, flags, null, [||], culture, [||])
        handle.Unwrap() :?> 'T

    /// Application Domain load state
    type AppDomainLoadInfo<'Manager when 'Manager :> IAppDomainManager 
                                    and 'Manager :> MarshalByRefObject 
                                    and 'Manager : (new : unit -> 'Manager)> =
        {
            /// AppDomain identifier
            Id : string
            /// AppDomain instance
            AppDomain : AppDomain
            /// AppDomain manager instance
            Manager : 'Manager
            /// Declared Vagabond dependency load state
            Dependencies : Map<string, AssemblyId>
        }
    with
        /// Initialize an AppDomain instance with given Vagabond dependencies
        static member Init (?dependencies : seq<AssemblyId>) =
            let id = Guid.NewGuid().ToString()
            let dependencies = 
                match dependencies with
                | None -> Map.empty
                | Some ds -> ds |> Seq.map (fun d -> d.FullName, d) |> Map.ofSeq

            let appDomain = initAppDomain id
            let manager = initAppDomainObject<'Manager> appDomain
            do manager.Initialize ()

            { Id = id; Manager = manager; AppDomain = appDomain; Dependencies = dependencies }

        member adli.AddDependencies (dependencies : seq<AssemblyId>) =
            { adli with Dependencies = Map.addMany (dependencies |> Seq.map (fun d -> d.FullName, d)) adli.Dependencies }


    /// Globad AppDomain load state
    type AppDomainPoolInfo<'Manager when 'Manager :> IAppDomainManager 
                                         and 'Manager :> MarshalByRefObject 
                                         and 'Manager : (new : unit -> 'Manager)> =
        {
            /// Application domain pool
            DomainPool : Map<string, AppDomainLoadInfo<'Manager>>
            /// Maximum concurrent domains
            MaxConcurrentDomains : int
            /// Minimum concurrent domains
            MinConcurrentDomains : int
            /// Max tasks allowed per domain
            MaxTasksPerDomain : int option
            /// Unload threshold: minimum timespan needed for unloading unused domains
            UnloadThreshold : TimeSpan option
        }
    with
        /// Initialize a new AppDomain with provided dependencies and add to state
        member s.AddNew(?dependencies) =
            let adli = AppDomainLoadInfo<'Manager>.Init (?dependencies = dependencies)
            adli, s.AddDomain adli

        /// Add existing or updated AppDomain info to state
        member s.AddDomain (adli : AppDomainLoadInfo<'Manager>) =
            { s with DomainPool = s.DomainPool.Add(adli.Id, adli) }

        /// Initialize a new AppDomain pool state
        static member Init(minimumConcurrentDomains : int, maximumConcurrentDomains : int, threshold : TimeSpan option, maxTasks : int option) =
            let mutable empty = {
                DomainPool = Map.empty
                MaxConcurrentDomains = maximumConcurrentDomains
                MinConcurrentDomains = minimumConcurrentDomains
                MaxTasksPerDomain = maxTasks
                UnloadThreshold = threshold
            }

            // populate pool with minimum allowed number of domains
            for _ in 1 .. minimumConcurrentDomains do
                empty <- empty.AddNew() |> snd

            empty

    /// try locating an AppDomain from pool that is compatible with supplied dependencies.
    let tryGetMatchingAppDomain (state : AppDomainPoolInfo<'M>) (dependencies : AssemblyId []) : AppDomainLoadInfo<'M> option * AppDomainPoolInfo<'M> =
        // return Some i if compatible, where i indicates the number of missing assemblies
        // and None if incompatible
        let getCompatibility (adli : AppDomainLoadInfo<'M>) =
            let missingCount = ref 0
            let isCompatibleAssembly (id : AssemblyId) =
                if state.MaxTasksPerDomain |> Option.exists (fun mtpd -> adli.Manager.TaskCount >= mtpd) 
                then false
                else
                    match adli.Dependencies.TryFind id.FullName with
                    | None -> incr missingCount ; true
                    | Some id' -> id = id'

            if Array.forall isCompatibleAssembly dependencies then Some (adli, !missingCount)
            else None

        let compatibleAppDomains = state.DomainPool |> Seq.choose(function (KeyValue(_,adli)) -> getCompatibility adli) |> Seq.toArray
        match compatibleAppDomains with
        | [||] -> None, state
        | _ -> 
            // locate best compatible AppDomain, minimizing by number of missing assemblies
            let adli = compatibleAppDomains |> Array.minBy snd |> fst
            // append dependencies to load state
            let adli2 = adli.AddDependencies dependencies
            let state2 = state.AddDomain adli2
            Some adli2, state2

    /// unloads provided application domains
    let disposeDomains (domains : seq<AppDomainLoadInfo<'M>>) = async {
        do for adli in domains do
            try 
                try adli.Manager.Finalize()
                finally AppDomain.Unload adli.AppDomain 
            with _ -> ()
    }

    /// <summary>
    ///     clean up state from AppDomains needing removal.
    /// </summary>
    /// <param name="minRemovals">Minimum removals required for success.</param>
    /// <param name="state">Input state.</param>
    let cleanupDomains (minRemovals : int) (state : AppDomainPoolInfo<'M>) : AppDomainPoolInfo<'M> =
        let removed = new ResizeArray<AppDomainLoadInfo<'M>> ()
        let mutable pool = state.DomainPool
        let now = DateTime.Now

        /// locate inactive domains, sorting by number of loaded assemblies
        let unusedDomains =         
            state.DomainPool
            |> Seq.map(function KeyValue(_,adli) -> adli)
            |> Seq.filter(fun adli -> adli.Manager.TaskCount = 0)
            |> Seq.sortBy(fun adli -> adli.Dependencies.Count)

        for adli in unusedDomains do
            if pool.Count <= state.MinConcurrentDomains then ()
            elif removed.Count < minRemovals || state.UnloadThreshold |> Option.exists(fun th -> now - adli.Manager.LastUsed > th) then
                removed.Add adli
                pool <- pool.Remove adli.Id

        if removed.Count < minRemovals then
            let msg = sprintf "AppDomain threshold of %d instances has been reached." state.MaxConcurrentDomains
            raise <| new OutOfResourcesException(msg)
        
        // asynchronously unload removed domains
        Async.Start(disposeDomains removed)

        { state with DomainPool = pool }

    /// get an Application domain appropriate for provided dependencies
    let getMatchingAppDomain (state : AppDomainPoolInfo<'M>) (dependencies : AssemblyId []) =
        match tryGetMatchingAppDomain state dependencies with
        | Some adli, state  -> state, adli
        | None, state ->
            let excessDomains = state.DomainPool.Count - state.MaxConcurrentDomains + 1
            let state2 =
                if excessDomains > 0 then cleanupDomains excessDomains state
                else state

            let adli, state3 = state2.AddNew(dependencies)
            state3, adli


    type AppDomainPoolMsg<'Manager when 'Manager :> IAppDomainManager 
                                     and 'Manager :> MarshalByRefObject 
                                     and 'Manager : (new : unit -> 'Manager)> =

        | GetDomain of dependencies : AssemblyId [] * ReplyChannel<'Manager>
        | GetState of ReplyChannel<AppDomainPoolInfo<'Manager>>
        | Dispose of ReplyChannel<unit>
        | Cleanup

    /// AppDomainManager actor behaviour
    let rec behaviour (gstate : AppDomainPoolInfo<'M> option) (self : MailboxProcessor<AppDomainPoolMsg<'M>>) = async {
        let! msg = self.Receive()

        match gstate with
        | None ->
            let e = new ObjectDisposedException("AppDomainIsolationManager")
            match msg with
            | GetDomain(_,rc) -> rc.ReplyWithError e
            | GetState rc -> rc.ReplyWithError e
            | _ -> ()
            return! behaviour gstate self

        | Some state ->
            match msg with
            | GetDomain(dependencies, rc) ->
                match Exn.protect2 getMatchingAppDomain state dependencies with
                | Success(state2, adli) -> rc.Reply adli.Manager ; return! behaviour (Some state2) self
                | Error e -> rc.ReplyWithError e ; return! behaviour gstate self

            | GetState rc -> rc.Reply state ; return! behaviour gstate self
            | Dispose rc ->
                state.DomainPool |> Seq.map (function KeyValue(_,adli) -> adli) |> disposeDomains |> Async.Start
                rc.Reply (())
                return! behaviour None self

            | Cleanup ->
                match Exn.protect2 cleanupDomains 0 state with
                | Success state2 -> return! behaviour (Some state2) self
                | Error e -> return! behaviour gstate self
    }


/// Provides an AppDomain pooling mechanism for use by Vagabond.
/// AppDomains are managed based what assembly dependencies are required for execution.
type AppDomainPool<'Manager when 'Manager :> IAppDomainManager 
                             and 'Manager :> MarshalByRefObject 
                             and 'Manager : (new : unit -> 'Manager)>

    internal (minimumConcurrentDomains : int, maximumConcurrentDomains : int, threshold : TimeSpan option, maxTasks : int option) =

    let cts = new CancellationTokenSource()
    let state = AppDomainPoolInfo<'Manager>.Init(minimumConcurrentDomains, maximumConcurrentDomains, threshold, maxTasks)
    let mbox = MailboxProcessor.Start(behaviour (Some state))
    let getState () = mbox.PostAndReply GetState

    // initialize a collector workflow if requested
    do
        match threshold with
        | None -> ()
        | Some ts ->
            let sleepInterval = min (int ts.TotalMilliseconds / 2) 10000
            let rec collector () = async {
                do! Async.Sleep sleepInterval
                try mbox.Post AppDomainPoolMsg<'Manager>.Cleanup with _ -> ()
                return! collector ()
            }

            do Async.Start(collector (), cts.Token)

    /// <summary>
    ///     Returns a marshalled Manager object attached to an AppDomain
    ///     that is compatible with state assembly dependencies 
    /// </summary>
    /// <param name="dependencies">Assembly dependencies required of AppDomain.</param>
    member __.RequestAppDomain(dependencies : seq<AssemblyId>) : 'Manager =
        let conflict =
            dependencies 
            |> Seq.groupBy(fun d -> d.FullName)
            |> Seq.tryPick(fun (name,ids) -> if Seq.length ids > 1 then Some name else None)

        match conflict with
        | None -> mbox.PostAndReply (fun ch -> GetDomain(Seq.toArray dependencies, ch))
        | Some c ->
            let msg = sprintf "Found conflicted assembly inputs in '%s'" c
            invalidArg "dependencies" msg

    /// Current AppDomain count
    member __.DomainCount = getState().DomainPool.Count
    /// Maximum AppDomain count
    member __.MaxDomains = getState().MaxConcurrentDomains
    /// Minimum AppDomain count
    member __.MinDomains = getState().MinConcurrentDomains

#if DEBUG
    member __.State = 
        getState().DomainPool 
        |> Seq.map (function KeyValue(_,v) -> v.Manager, v.AppDomain)
        |> Seq.toArray
#endif

    interface IDisposable with
        member __.Dispose () =
            if not cts.IsCancellationRequested then
                do mbox.PostAndReply AppDomainPoolMsg<'Manager>.Dispose
                cts.Cancel()

/// Provides an AppDomain pooling mechanism for use by Vagabond.
/// AppDomains are managed based what assembly dependencies are required for execution.
type AppDomainPool =
    /// <summary>
    ///     Creates a new AppDomain pool instance.
    /// </summary>
    /// <param name="minimumConcurrentDomains">Minimum allowed AppDomains. Defaults to 3.</param>
    /// <param name="maximumConcurrentDomains">Maximum allowed AppDomains. Defaults to 20.</param>
    /// <param name="threshold">TimeSpan after which unused AppDomains may get unloaded. Defaults to infinite.</param>
    /// <param name="maxTasksPerDomain">Maximum number of tasks allowed per domain. Defaults to infinite.</param>
    static member Create<'Manager when 'Manager :> IAppDomainManager 
                                 and 'Manager :> MarshalByRefObject 
                                 and 'Manager : (new : unit -> 'Manager)>
        (?minimumConcurrentDomains : int, ?maximumConcurrentDomains : int, ?threshold : TimeSpan, ?maxTasksPerDomain : int) =

        let minimumConcurrentDomains = defaultArg minimumConcurrentDomains 3
        let maximumConcurrentDomains = defaultArg maximumConcurrentDomains 20
        if minimumConcurrentDomains < 0 then invalidArg "minimumConcurrentDomains" "Must be non-negative."
        if maximumConcurrentDomains < minimumConcurrentDomains then invalidArg "minimumConcurrentDomains" "should be greater or equal to 'minimumConcurrentDomains'."
        if maxTasksPerDomain |> Option.exists (fun mtpd -> mtpd <= 0) then invalidArg "maxTasksPerDomain" "should be positive."
        new AppDomainPool<'Manager>(minimumConcurrentDomains, maximumConcurrentDomains, threshold, maxTasksPerDomain)