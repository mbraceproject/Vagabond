namespace Nessos.Vagabond.AppDomainPool

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization
open System.Runtime.Remoting
open System.Security
open System.Security.Permissions
open System.Threading
open System.Threading.Tasks

open Microsoft.FSharp.Control

open Nessos.FsPickler
open Nessos.Vagabond

[<AutoSerializable(true)>] 
type OutOfResourcesException =
    inherit Exception
    internal new (message : string) = { inherit Exception(message) }
    private new (sI : SerializationInfo, sc : StreamingContext) =  { inherit Exception(sI, sc) }

/// User-defined configuration object
/// passed at AppDomain initialization
/// Values are instantiated on the client appdomain
/// and marshalled to the pooled domains
type IAppDomainConfiguration = interface end

/// Interface implemented by MarshalByRef types
/// used for managing Application domains.
/// Types implementing the interface must carry
/// a parameterless constructor.
type IAppDomainManager =
    /// AppDomain initializer method.
    abstract Initialize : IAppDomainConfiguration -> unit
    /// AppDomain finalization method.
    abstract Finalize : unit -> unit
    /// Gets the number of tasks currently active in the AppDomain. 
    /// AppDomains with non-zero task count will never be unloaded
    /// by the AppDomainPool.
    abstract TaskCount : int
    /// Gets the last time this app domain instance was used.
    /// Used by the AppDomainPool to unload unused instances.
    abstract LastUsed : DateTime

[<AutoOpen>]
module private Impl =

    /// initialize a new AppDomain with given friendly name
    let initAppDomain permissions (name : string) =
        let currentDomain = AppDomain.CurrentDomain
        let appDomainSetup = currentDomain.SetupInformation
        let permissions = defaultArg permissions currentDomain.PermissionSet
        let evidence = new Security.Policy.Evidence(currentDomain.Evidence)
        AppDomain.CreateDomain(name, evidence, appDomainSetup, permissions)

    let ctorFlags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance

    let activate<'T> () = typeof<'T>.GetConstructor(ctorFlags, null, [||], [||]).Invoke([||]) :?> 'T

    /// initialize an object in the target Application domain
    let initAppDomainObject<'T when 'T :> MarshalByRefObject 
                                and 'T : (new : unit -> 'T)> (targetDomain : AppDomain) =

        let assemblyName = typeof<'T>.Assembly.FullName
        let typeName = typeof<'T>.FullName
        let culture = System.Globalization.CultureInfo.CurrentCulture
        let handle = targetDomain.CreateInstance(assemblyName, typeName, false, ctorFlags, null, [||], culture, [||])
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
        static member Init (config : IAppDomainConfiguration, permissions : PermissionSet option, dependencies : seq<AssemblyId>) =
            let id = Guid.NewGuid().ToString()
            let dependencies = dependencies |> Seq.map (fun d -> d.FullName, d) |> Map.ofSeq
            let appDomain = initAppDomain permissions id
            let manager = initAppDomainObject<'Manager> appDomain
            do manager.Initialize config

            { Id = id; Manager = manager; AppDomain = appDomain; Dependencies = dependencies }

        member adli.AddDependencies (dependencies : seq<AssemblyId>) =
            { adli with Dependencies = dependencies |> Seq.map (fun d -> d.FullName, d) |> Map.addMany adli.Dependencies }


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
            /// AppDomain permission set
            Permissions : PermissionSet option
            /// User-specified configuration for the AppDomain
            Configuration : IAppDomainConfiguration
            /// Unload threshold: minimum timespan needed for unloading unused domains
            UnloadThreshold : TimeSpan option
        }
    with
        /// Initialize a new AppDomain with provided dependencies and add to state
        member s.AddNew(dependencies) =
            let adli = AppDomainLoadInfo<'Manager>.Init (s.Configuration, dependencies = dependencies, permissions = s.Permissions)
            adli, s.AddDomain adli

        /// Add existing or updated AppDomain info to state
        member s.AddDomain (adli : AppDomainLoadInfo<'Manager>) =
            { s with DomainPool = s.DomainPool.Add(adli.Id, adli) }

        /// Initialize a new AppDomain pool state
        static member Init(minimumConcurrentDomains : int, maximumConcurrentDomains : int, config : IAppDomainConfiguration ,threshold : TimeSpan option, maxTasks : int option, permissions : PermissionSet option) =
            {
                DomainPool = Map.empty
                MaxConcurrentDomains = maximumConcurrentDomains
                MinConcurrentDomains = minimumConcurrentDomains
                Permissions = permissions
                Configuration = config
                MaxTasksPerDomain = maxTasks
                UnloadThreshold = threshold
            }

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
        | PopulateInitialDomains
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
            | PopulateInitialDomains ->
                let N = state.MinConcurrentDomains - state.DomainPool.Count
                if N > 0 then
                    try
                        let state = ref state
                        do for i = 1 to N do
                            let _, state' = state.Value.AddNew []
                            state := state'

                        return! behaviour (Some !state) self

                    with _ -> return! behaviour gstate self
                else
                    return! behaviour gstate self
                
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
[<AutoSerializable(false)>]
type AppDomainPool<'Manager when 'Manager :> IAppDomainManager 
                             and 'Manager :> MarshalByRefObject 
                             and 'Manager : (new : unit -> 'Manager)>

    internal (minimumConcurrentDomains : int, maximumConcurrentDomains : int, config : IAppDomainConfiguration, threshold : TimeSpan option, maxTasks : int option, permissions : PermissionSet option) =

    let cts = new CancellationTokenSource()
    let state = AppDomainPoolInfo<'Manager>.Init(minimumConcurrentDomains, maximumConcurrentDomains, config, threshold, maxTasks, permissions)
    let mbox = MailboxProcessor.Start(behaviour (Some state))
    do mbox.Post PopulateInitialDomains
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
    ///     Returns a marshalled Manager object attached to a pooled AppDomain instance.
    ///     AppDomain will be selected based on dependency affinity.
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
    /// <param name="configuration">Configuration object passed to the pooled AppDomains.</param>
    /// <param name="minimumConcurrentDomains">Minimum allowed AppDomains. Defaults to 3.</param>
    /// <param name="maximumConcurrentDomains">Maximum allowed AppDomains. Defaults to 20.</param>
    /// <param name="threshold">TimeSpan after which unused AppDomains may get unloaded. Defaults to infinite.</param>
    /// <param name="maxTasksPerDomain">Maximum number of tasks allowed per domain. Defaults to infinite.</param>
    /// <param name="permissions">AppDomain pool permissions set. Defaults to permission set of current domain.</param>
    static member Create<'Manager when 'Manager :> IAppDomainManager 
                                   and 'Manager :> MarshalByRefObject 
                                   and 'Manager : (new : unit -> 'Manager)>
        (configuration : IAppDomainConfiguration, ?minimumConcurrentDomains : int, ?maximumConcurrentDomains : int, ?threshold : TimeSpan, ?maxTasksPerDomain : int, ?permissions : PermissionSet) =

        let minimumConcurrentDomains = defaultArg minimumConcurrentDomains 3
        let maximumConcurrentDomains = defaultArg maximumConcurrentDomains 20
        if minimumConcurrentDomains < 0 then invalidArg "minimumConcurrentDomains" "Must be non-negative."
        if maximumConcurrentDomains < minimumConcurrentDomains then invalidArg "minimumConcurrentDomains" "should be greater or equal to 'minimumConcurrentDomains'."
        if maxTasksPerDomain |> Option.exists (fun mtpd -> mtpd <= 0) then invalidArg "maxTasksPerDomain" "should be positive."
        new AppDomainPool<'Manager>(minimumConcurrentDomains, maximumConcurrentDomains, configuration, threshold, maxTasksPerDomain, permissions)


    /// <summary>
    ///     Creates a new AppDomain pool instance.
    /// </summary>
    /// <param name="minimumConcurrentDomains">Minimum allowed AppDomains. Defaults to 3.</param>
    /// <param name="maximumConcurrentDomains">Maximum allowed AppDomains. Defaults to 20.</param>
    /// <param name="threshold">TimeSpan after which unused AppDomains may get unloaded. Defaults to infinite.</param>
    /// <param name="maxTasksPerDomain">Maximum number of tasks allowed per domain. Defaults to infinite.</param>
    /// <param name="permissions">AppDomain pool permissions set. Defaults to permission set of current domain.</param>
    static member Create<'Manager, 'Config 
                                when 'Manager :> IAppDomainManager 
                                 and 'Manager :> MarshalByRefObject 
                                 and 'Manager : (new : unit -> 'Manager)
                                 and 'Config :> IAppDomainConfiguration
                                 and 'Config : (new : unit -> 'Config)>
        (?minimumConcurrentDomains : int, ?maximumConcurrentDomains : int, ?threshold : TimeSpan, ?maxTasksPerDomain : int, ?permissions : PermissionSet) =

        AppDomainPool.Create<'Manager>(activate<'Config>(), 
            ?minimumConcurrentDomains = minimumConcurrentDomains, ?maximumConcurrentDomains = maximumConcurrentDomains, 
            ?threshold = threshold, ?maxTasksPerDomain = maxTasksPerDomain, ?permissions = permissions)


//
//  Lambda Evaluator AppDomain pool: application of the AppDomain pool that uses FsPickler for sending lambdas 
//  or asynchronous workflows for execution in pooled AppDomains
//

[<AutoOpen>]
module private EvaluatorImpl =
    
    
    // BinaryFormatter, the .NET default in remoting is not to be trusted when serializing closures
    // use FsPickler instead and pass Pickle<'T> values to the marshalled objects
    let pickler = lazy(FsPickler.CreateBinary())

    type ResultPickle = Pickle<Exn<obj>>

    [<AbstractClass>]
    type ImmortalMarshalByRefObject () =
        inherit MarshalByRefObject ()
        [<SecurityPermissionAttribute(SecurityAction.Demand, Flags = SecurityPermissionFlag.Infrastructure)>]
        override __.InitializeLifetimeService() = null
        member o.Disconnect () = ignore <| RemotingServices.Disconnect o

    /// Marshalled task completion source for async computation over AppDomains
    type MarshalledTaskCompletionSource<'T> () =
        inherit ImmortalMarshalByRefObject ()
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
        inherit ImmortalMarshalByRefObject ()
        let cts = new CancellationTokenSource()
        /// Cancel the cancellation token source
        member __.Cancel() = cts.Cancel()
        /// Gets the local cancellation token
        member __.CancellationToken = cts.Token

    /// AppDomain configuration that carries a pickled initializer
    type AppDomainEvaluatorConfiguration(?initializer : unit -> unit) =
        let pInitializer = initializer |> Option.map pickler.Value.PickleTyped 
        member __.Initializer = pInitializer
        interface IAppDomainConfiguration

    /// AppDomain managing object
    type AppDomainEvaluatorManager() =
        inherit ImmortalMarshalByRefObject()

        let mutable taskCount = 0
        let mutable lastUsed = DateTime.Now

        let init () =
            let _ = Interlocked.Increment &taskCount
            lastUsed <- DateTime.Now

        let fini () =
            let _ = Interlocked.Decrement &taskCount
            lastUsed <- DateTime.Now

        /// Synchronously executes a computation in remote AppDomain
        member e.EvaluateSync(pFunc : Pickle<unit -> obj>) : ResultPickle =
            let func = pickler.Value.UnPickleTyped pFunc
            init ()
            try
                let result = Exn.protect func ()
                pickler.Value.PickleTyped result
            finally fini ()

        /// Asynchronously executes a computation in remote AppDomain
        member e.EvaluateAsync(mtcs : MarshalledTaskCompletionSource<ResultPickle>, pWorkflow : Pickle<Async<obj>>) : MarshalledCancellationTokenSource =
            let mcts = new MarshalledCancellationTokenSource()
            let workflow = pickler.Value.UnPickleTyped pWorkflow
            let setResult (result : Exn<obj>) =
                try
                    let presult = 
                        try pickler.Value.PickleTyped result
                        with ex -> pickler.Value.PickleTyped (Error ex)

                    mtcs.SetResult presult
                finally 
                    mcts.Disconnect()
                    fini ()

            let setCancelled _ = try mtcs.SetCanceled() finally fini ()

            let exec () = Async.StartWithContinuations(workflow, Success >> setResult, Error >> setResult, setCancelled, mcts.CancellationToken)

            init ()
            Async.Start(async { exec () })
            mcts

        interface IAppDomainManager with
            member e.Initialize(config : IAppDomainConfiguration) =
                match config with
                | :? AppDomainEvaluatorConfiguration as c ->
                    c.Initializer |> Option.iter (fun init -> pickler.Value.UnPickleTyped(init)())
                | _ -> ()

            member e.Finalize () = ()
            member e.TaskCount = taskCount
            member e.LastUsed = lastUsed

    let evalSync (mgr : AppDomainEvaluatorManager) (f : unit -> 'T) : 'T =
        let result = (fun () -> f () :> obj) |> pickler.Value.PickleTyped |> mgr.EvaluateSync |> pickler.Value.UnPickleTyped
        result.Value :?> 'T

    let evalAsync (mgr : AppDomainEvaluatorManager) (f : Async<'T>) : Async<'T> = async {
        let! ct = Async.CancellationToken
        let pworkflow = pickler.Value.PickleTyped(async { let! r = f in return box r })
        let mtcs = new MarshalledTaskCompletionSource<ResultPickle>()
        try
            let mcts = mgr.EvaluateAsync(mtcs, pworkflow)
            use d = ct.Register (fun () -> mcts.Cancel ())
            let! presult = Async.AwaitTask(mtcs.Task)
            let result = pickler.Value.UnPickleTyped presult
            return result.Value :?> 'T
        finally
            mtcs.Disconnect()
    }
        

/// Defines an app domain pool that evaluates code based on Vagabond dependency affinities
[<AutoSerializable(false)>]
type AppDomainEvaluatorPool private (pool : AppDomainPool<AppDomainEvaluatorManager>) =

    /// <summary>
    ///     Creates a new AppDomainEvaluator instance.
    /// </summary>
    /// <param name="appDomainInitializer">Initializer run on every AppDomain upon creation. Defaults to none.</param>
    /// <param name="minimumConcurrentDomains">Minimum allowed AppDomains. Defaults to 3.</param>
    /// <param name="maximumConcurrentDomains">Maximum allowed AppDomains. Defaults to 20.</param>
    /// <param name="threshold">TimeSpan after which unused AppDomains may get unloaded. Defaults to infinite.</param>
    /// <param name="maxTasksPerDomain">Maximum number of tasks allowed per domain. Defaults to infinite.</param>
    /// <param name="permissions">AppDomain pool permissions set. Defaults to permission set of current domain.</param>
    static member Create(?appDomainInitializer : unit -> unit, ?minimumConcurrentDomains : int, ?maximumConcurrentDomains : int, ?threshold : TimeSpan, ?maxTasksPerDomain : int, ?permissions : PermissionSet) =
        let config = new AppDomainEvaluatorConfiguration(?initializer = appDomainInitializer)
        let pool = AppDomainPool.Create<AppDomainEvaluatorManager>(config, ?threshold = threshold, ?maxTasksPerDomain = maxTasksPerDomain,
                            ?minimumConcurrentDomains = minimumConcurrentDomains, ?maximumConcurrentDomains = maximumConcurrentDomains, ?permissions = permissions)

        new AppDomainEvaluatorPool(pool)

    /// Current AppDomain count
    member __.DomainCount = pool.DomainCount
    /// Maximum allowed AppDomain count
    member __.MaxDomains = pool.MaxDomains
    /// Minimum allowed AppDomain count
    member __.MinDomains = pool.MinDomains
    
    /// <summary>
    ///     Evaluates function in pooled AppDomain. AppDomain will be selected based on dependency affinity.
    /// </summary>
    /// <param name="dependencies">Dependencies for operation.</param>
    /// <param name="f">Function to be executed.</param>
    member __.Evaluate(dependencies : seq<AssemblyId>, f : unit -> 'T) : 'T =
        let evaluator = pool.RequestAppDomain dependencies
        evalSync evaluator f

    /// <summary>
    ///     Asynchronously evaluates function in pooled AppDomain. AppDomain will be selected based on dependency affinity.
    /// </summary>
    /// <param name="dependencies">Dependencies for operation.</param>
    /// <param name="f">Asynchronous workflow to be executed.</param>
    member __.EvaluateAsync(dependencies : seq<AssemblyId>, f : Async<'T>) : Async<'T> = async {
        let evaluator = pool.RequestAppDomain dependencies
        return! evalAsync evaluator f
    }

    interface IDisposable with
        member __.Dispose () = (pool :> IDisposable).Dispose()