namespace MBrace.Vagabond.LoadContextPool

open System
open System.Reflection
#if !NETSTANDARD
open System.Runtime.Loader
#endif
open System.Runtime.Serialization
open System.Threading
open System.Threading.Tasks

open Microsoft.FSharp.Control

open MBrace.FsPickler
open MBrace.Vagabond

[<AutoSerializable(true)>] 
type OutOfResourcesException =
    inherit Exception
    internal new (message : string) = { inherit Exception(message) }
    private new (sI : SerializationInfo, sc : StreamingContext) =  { inherit Exception(sI, sc) }

/// Interface used for managing LoadContext instances.
type ILoadContextManager =
    inherit IDisposable
    /// AssemblyLoadContext initializer method.
    abstract Initialize : configuration:obj -> unit
    /// Gets the number of tasks currently active in the LoadContext. 
    /// Contexts with non-zero task count will never be unloaded
    abstract TaskCount : int
    /// Gets the last time this load context was used.
    /// Used by the AssemblyLoadContext to cull unused instances.
    abstract LastUsed : DateTime

/// Proxy interface used for communicating with type loaded in remote AssemblyLoadContext
type ILoadContextProxy<'T when 'T :> IDisposable> =
    inherit IDisposable
    /// AssemblyLoadContext hosting the instance
    abstract LoadContext : AssemblyLoadContext
    /// Marshalls a command which will be executed in the remote AssemblyLoadContext
    abstract Execute : command:('T -> 'R) -> 'R
    /// Marshalls a command which will be executed in the remote AssemblyLoadContext
    abstract ExecuteAsync : command:('T -> Async<'R>) -> Async<'R>

[<AutoOpen>]
module private Impl =

    let ctorFlags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance
    let activate (t:Type) = t.GetConstructor(ctorFlags, null, [||], [||]).Invoke([||])

    type private LoadContextMarshaller<'T when 'T : (new : unit -> 'T) and 'T :> IDisposable> () =
        let instance = new 'T()

        static let pickler = FsPickler.CreateBinarySerializer()

        interface IDisposable with member __.Dispose() = instance.Dispose()

        member __.ExecuteMarshalled (bytes : byte[]) : byte[] = 
            let result = 
                try 
                    let command = pickler.UnPickle<'T -> obj> bytes
                    command instance |> Choice1Of2 
                with e -> Choice2Of2 e

            pickler.Pickle<Choice<obj,exn>> result

        member __.ExecuteMarshalledAsync (ct : CancellationToken, bytes : byte[]) : Task<byte[]> = 
            let work = async {
                let executor = async { 
                    let command = pickler.UnPickle<'T -> Async<obj>> bytes 
                    return! command instance 
                }

                let! result = Async.Catch executor
                return pickler.Pickle<Choice<obj, exn>> result
            } 
    
            Async.StartAsTask(work, cancellationToken = ct)

        static member CreateProxyFromMarshallerHandle (context : AssemblyLoadContext, remoteHandle : obj) =
            let remoteMethod = remoteHandle.GetType().GetMethod("ExecuteMarshalled", BindingFlags.NonPublic ||| BindingFlags.Instance)
            let remoteAsyncMethod = remoteHandle.GetType().GetMethod("ExecuteMarshalledAsync", BindingFlags.NonPublic ||| BindingFlags.Instance)
            { new ILoadContextProxy<'T> with
                member __.LoadContext = context
                member __.Dispose() = (remoteHandle :?> IDisposable).Dispose()
                member __.Execute<'R> (command : 'T -> 'R) =
                    let boxedCommand instance = let result = command instance in result :> obj
                    let commandBytes = pickler.Pickle<'T -> obj> boxedCommand
                    let responseBytes = remoteMethod.Invoke(remoteHandle, [|commandBytes|]) :?> byte[]
                    match pickler.UnPickle<Choice<obj,exn>> responseBytes with
                    | Choice1Of2 obj -> obj :?> 'R
                    | Choice2Of2 e -> reraise' e

                member __.ExecuteAsync<'R> (command : 'T -> Async<'R>) = async {
                    let! ct = Async.CancellationToken
                    let boxedCommand instance = async { let! result = command instance in return box result }
                    let commandBytes = pickler.Pickle<'T -> Async<obj>> boxedCommand
                    let responseTask = remoteAsyncMethod.Invoke(remoteHandle, [|ct ; commandBytes|]) :?> Task<byte[]>
                    let! responseBytes = Async.AwaitTask responseTask
                    return 
                        match pickler.UnPickle<Choice<obj, exn>> responseBytes with
                        | Choice1Of2 obj -> obj :?> 'R
                        | Choice2Of2 e -> reraise' e
                }
            }

    type AssemblyLoadContext with
        member ctx.CreateProxy<'T when 'T : (new : unit -> 'T) and 'T :> IDisposable>() : ILoadContextProxy<'T> =
            let getRemoteType (t : Type) =
                let remoteAssembly = ctx.LoadFromAssemblyPath t.Assembly.Location
                remoteAssembly.GetType t.FullName

            // Construct the type of LoadContextMarshaller<'TProxy>, but using assemblies loaded in the remote context
            let remoteProxyType = getRemoteType typeof<'T>
            let remoteMarshallerType = getRemoteType (typeof<LoadContextMarshaller<'T>>.GetGenericTypeDefinition())
            let remoteInstanceType = remoteMarshallerType.MakeGenericType remoteProxyType

            // instantiate proxy in remote context
            let remoteInstance = activate remoteInstanceType
            LoadContextMarshaller<'T>.CreateProxyFromMarshallerHandle(ctx, remoteInstance)

    /// An assembly load context that mirrors assembly loading from the currently running context
    type MirroredAssemblyLoadContext(name : string) =
        inherit AssemblyLoadContext(name, isCollectible = true)

        let tryResolveFileName (an : AssemblyName) =
            let isMatchingAssembly (assembly : Assembly) =
                let can = assembly.GetName()
                can.Name = an.Name &&
                can.Version >= an.Version &&
                can.GetPublicKeyToken() = an.GetPublicKeyToken()

            Utils.currentLoadContext.Assemblies
            |> Seq.filter (fun a -> not a.IsDynamic && not (String.IsNullOrEmpty a.Location))
            |> Seq.tryFind isMatchingAssembly
            |> Option.map (fun a -> a.Location)
                
        override this.Load an =
            match tryResolveFileName an with
            | None -> null
            | Some path -> this.LoadFromAssemblyPath path

    /// AssemblyLoadContext state
    type AssemblyLoadContextInfo<'Manager when 'Manager :> ILoadContextManager
                                     and 'Manager : (new : unit -> 'Manager)> =
        {
            /// Load Context identifier
            Id : string
            /// Load Context instance
            LoadContext : MirroredAssemblyLoadContext
            /// Load Context manager proxy instance
            Proxy : ILoadContextProxy<'Manager>
            /// Declared Vagabond dependency load state
            Dependencies : Map<string, AssemblyId>
        }
    with
        /// Initialize an LoadContext instance with given Vagabond dependencies
        static member Init (configuration : obj, dependencies : seq<AssemblyId>) =
            let id = Guid.NewGuid().ToString()
            let dependencies = dependencies |> Seq.map (fun d -> d.FullName, d) |> Map.ofSeq
            let loadContext = new MirroredAssemblyLoadContext(name = id)
            let proxy = loadContext.CreateProxy<'Manager>()
            proxy.Execute(fun m -> m.Initialize configuration)

            { Id = id; Proxy = proxy; LoadContext = loadContext; Dependencies = dependencies }

        member adli.AddDependencies (dependencies : seq<AssemblyId>) =
            { adli with Dependencies = dependencies |> Seq.map (fun d -> d.FullName, d) |> Map.addMany adli.Dependencies }


    /// Load Context pool state
    type LoadContextPoolInfo<'Manager when 'Manager :> ILoadContextManager
                                       and 'Manager : (new : unit -> 'Manager)> =
        {
            /// Load Context pool
            LoadContextPool : Map<string, AssemblyLoadContextInfo<'Manager>>
            /// Maximum concurrent load contexts
            MaxConcurrentContexts : int
            /// Minimum concurrent load contexts
            MinConcurrentContexts : int
            /// Max tasks allowed per load context
            MaxTasksPerContext : int option
            /// User-specified configuration for the Load Context
            Configuration : obj
            /// Unload threshold: minimum timespan needed for unloading unused load Contexts
            UnloadThreshold : TimeSpan option
        }
    with
        /// Initialize a new LoadContext with provided dependencies and add to state
        member s.AddNew(dependencies) =
            let adli = AssemblyLoadContextInfo.Init (s.Configuration, dependencies = dependencies)
            adli, s.AddLoadContext adli

        /// Add existing or updated load context info to state
        member s.AddLoadContext (adli : AssemblyLoadContextInfo<'Manager>) =
            { s with LoadContextPool = s.LoadContextPool.Add(adli.Id, adli) }

        /// Initialize a new load context pool state
        static member Init(minimumConcurrentContexts : int, maximumConcurrentContexts : int, config : obj ,threshold : TimeSpan option, maxTasks : int option) =
            {
                LoadContextPool = Map.empty
                MaxConcurrentContexts = maximumConcurrentContexts
                MinConcurrentContexts = minimumConcurrentContexts
                Configuration = config
                MaxTasksPerContext = maxTasks
                UnloadThreshold = threshold
            }

    /// try locating a LoadContext from pool that is compatible with supplied dependencies.
    let tryGetMatchingLoadContext (state : LoadContextPoolInfo<'M>) (dependencies : AssemblyId []) : AssemblyLoadContextInfo<'M> option * LoadContextPoolInfo<'M> =
        // return Some i if compatible, where i indicates the number of missing assemblies
        // and None if incompatible
        let getCompatibility (adli : AssemblyLoadContextInfo<'M>) =
            let missingCount = ref 0
            let isCompatibleAssembly (id : AssemblyId) =
                if state.MaxTasksPerContext |> Option.exists (fun mtpd -> adli.Proxy.Execute(fun m -> m.TaskCount) >= mtpd) 
                then false
                else
                    match adli.Dependencies.TryFind id.FullName with
                    | None -> incr missingCount ; true
                    | Some id' -> id = id'

            if Array.forall isCompatibleAssembly dependencies then Some (adli, !missingCount)
            else None

        let compatibleLoadContexts = state.LoadContextPool |> Seq.choose(function (KeyValue(_,adli)) -> getCompatibility adli) |> Seq.toArray
        match compatibleLoadContexts with
        | [||] -> None, state
        | _ -> 
            // locate best compatible load contexts, minimizing by number of missing assemblies
            let adli = compatibleLoadContexts |> Array.minBy snd |> fst
            // append dependencies to load state
            let adli2 = adli.AddDependencies dependencies
            let state2 = state.AddLoadContext adli2
            Some adli2, state2

    /// unloads specified load contexts
    let disposeContexts (contexts : seq<AssemblyLoadContextInfo<'M>>) = async {
        do for adli in contexts do
            try 
                try adli.Proxy.Execute(fun m -> m.Dispose())
                finally adli.LoadContext.Unload()
            with _ -> ()
    }

    /// <summary>
    ///     clean up state from contexts needing removal.
    /// </summary>
    /// <param name="minRemovals">Minimum removals required for success.</param>
    /// <param name="state">Input state.</param>
    let cleanupContexts (minRemovals : int) (state : LoadContextPoolInfo<'M>) : LoadContextPoolInfo<'M> =
        let removed = new ResizeArray<AssemblyLoadContextInfo<'M>> ()
        let mutable pool = state.LoadContextPool
        let now = DateTime.Now

        /// locate inactive contexts, sorting by number of loaded assemblies
        let unusedContexts =         
            state.LoadContextPool
            |> Seq.map(function KeyValue(_,adli) -> adli)
            |> Seq.filter(fun adli -> adli.Proxy.Execute(fun m -> m.TaskCount = 0))
            |> Seq.sortBy(fun adli -> adli.Dependencies.Count)

        for adli in unusedContexts do
            if pool.Count <= state.MinConcurrentContexts then ()
            elif removed.Count < minRemovals || state.UnloadThreshold |> Option.exists(fun th -> now - adli.Proxy.Execute(fun m -> m.LastUsed) > th) then
                removed.Add adli
                pool <- pool.Remove adli.Id

        if removed.Count < minRemovals then
            let msg = sprintf "AssemblyLoadContext threshold of %d instances has been reached." state.MaxConcurrentContexts
            raise <| new OutOfResourcesException(msg)
        
        // asynchronously unload removed contexts
        Async.Start(disposeContexts removed)

        { state with LoadContextPool = pool }

    /// get a load context appropriate for provided dependencies
    let getMatchingLoadContext (state : LoadContextPoolInfo<'M>) (dependencies : AssemblyId []) =
        match tryGetMatchingLoadContext state dependencies with
        | Some adli, state  -> state, adli
        | None, state ->
            let excessContexts = state.LoadContextPool.Count - state.MaxConcurrentContexts + 1
            let state2 =
                if excessContexts > 0 then cleanupContexts excessContexts state
                else state

            let adli, state3 = state2.AddNew(dependencies)
            state3, adli


    type LoadContextPoolMsg<'Manager when 'Manager :> ILoadContextManager 
                                     and 'Manager : (new : unit -> 'Manager)> =
        | GetContext of dependencies : AssemblyId [] * ReplyChannel<ILoadContextProxy<'Manager>>
        | GetState of ReplyChannel<LoadContextPoolInfo<'Manager>>
        | Dispose of ReplyChannel<unit>
        | Cleanup

    /// LoadContextManager actor behaviour
    let rec behaviour (gstate : LoadContextPoolInfo<'M> option) (self : MailboxProcessor<LoadContextPoolMsg<'M>>) = async {
        let! msg = self.Receive()

        match gstate with
        | None ->
            let e = new ObjectDisposedException("LoadContextIsolationManager")
            match msg with
            | GetContext(_,rc) -> rc.ReplyWithError e
            | GetState rc -> rc.ReplyWithError e
            | _ -> ()
            return! behaviour gstate self

        | Some state ->
            match msg with
            | GetContext(dependencies, rc) ->
                match Exn.protect2 getMatchingLoadContext state dependencies with
                | Success(state2, adli) -> rc.Reply adli.Proxy ; return! behaviour (Some state2) self
                | Error e -> rc.ReplyWithError e ; return! behaviour gstate self

            | GetState rc -> rc.Reply state ; return! behaviour gstate self
            | Dispose rc ->
                state.LoadContextPool |> Seq.map (function KeyValue(_,adli) -> adli) |> disposeContexts |> Async.Start
                rc.Reply (())
                return! behaviour None self

            | Cleanup ->
                match Exn.protect2 cleanupContexts 0 state with
                | Success state2 -> return! behaviour (Some state2) self
                | Error _ -> return! behaviour gstate self
    }

/// Provides a AssemblyLoadContext pooling mechanism for use by Vagabond.
/// AssemblyLoadContexts are allocated based on what assembly dependencies are required for execution.
[<AutoSerializable(false)>]
type AssemblyLoadContextPool<'Manager when 'Manager :> ILoadContextManager
                             and 'Manager : (new : unit -> 'Manager)>

    internal (minimumConcurrentContexts : int, maximumConcurrentContexts : int, config : obj, threshold : TimeSpan option, maxTasks : int option) =

    let cts = new CancellationTokenSource()
    let state = LoadContextPoolInfo<'Manager>.Init(minimumConcurrentContexts, maximumConcurrentContexts, config, threshold, maxTasks)
    let mbox = MailboxProcessor.Start(behaviour (Some state))
    let getState () = mbox.PostAndReply GetState

    // initialize a collector workflow if requested
    do
        match threshold with
        | None -> ()
        | Some ts ->
            let sleepInterval = min (int ts.TotalMilliseconds / 2) 10000 |> max 100
            let rec collector () = async {
                do! Async.Sleep sleepInterval
                try mbox.Post LoadContextPoolMsg.Cleanup with _ -> ()
                return! collector ()
            }

            do Async.Start(collector (), cts.Token)

    /// <summary>
    ///     Returns a Manager object attached to a pooled AssemblyLoadContext instance.
    ///     AssemblyLoadContext will be selected based on dependency affinity.
    /// </summary>
    /// <param name="dependencies">Assembly dependencies required of AssemblyLoadContext.</param>
    member __.RequestLoadContext(dependencies : seq<AssemblyId>) : ILoadContextProxy<'Manager> =
        let conflict =
            dependencies 
            |> Seq.groupBy(fun d -> d.FullName)
            |> Seq.tryPick(fun (name,ids) -> if Seq.length ids > 1 then Some name else None)

        match conflict with
        | None -> mbox.PostAndReply (fun ch -> GetContext(Seq.toArray dependencies, ch))
        | Some c ->
            let msg = sprintf "Found conflicted assembly inputs in '%s'" c
            invalidArg "dependencies" msg

    /// Current AssemblyLoadContext count
    member __.LoadContextCount = getState().LoadContextPool.Count
    /// Maximum AssemblyLoadContext count
    member __.MaxLoadContexts = getState().MaxConcurrentContexts
    /// Minimum AssemblyLoadContext count
    member __.MinLoadContexts = getState().MinConcurrentContexts

#if DEBUG
    member __.State = 
        getState().LoadContextPool 
        |> Seq.map (function KeyValue(_,v) -> v.LoadContext :> AssemblyLoadContext, v.Proxy)
        |> Seq.toArray
#endif

    interface IDisposable with
        member __.Dispose () =
            if not cts.IsCancellationRequested then
                do mbox.PostAndReply LoadContextPoolMsg.Dispose
                cts.Cancel()

/// Provides an AssemblyLoadContext pooling mechanism for use by Vagabond.
/// AssemblyLoadContexts are allocated based what assembly dependencies are required for execution.
type AssemblyLoadContextPool =
    /// <summary>
    ///     Creates a new AssemblyLoadContext pool instance.
    /// </summary>
    /// <param name="configuration">Configuration object passed to newly created AssemblyLoadContexts.</param>
    /// <param name="minimumConcurrentContexts">Minimum allowed AssemblyLoadContexts. Defaults to 3.</param>
    /// <param name="maximumConcurrentContexts">Maximum allowed AssemblyLoadContexts. Defaults to 20.</param>
    /// <param name="threshold">TimeSpan after which unused AssemblyLoadContexts may get unloaded. Defaults to infinite.</param>
    /// <param name="maxTasksPerContext">Maximum number of tasks allowed per load context. Defaults to infinite.</param>
    static member Create<'Manager when 'Manager :> ILoadContextManager
                                   and 'Manager : (new : unit -> 'Manager)>
        (configuration : obj, ?minimumConcurrentContexts : int, ?maximumConcurrentContexts : int, ?threshold : TimeSpan, ?maxTasksPerContext : int) =

        let minimumConcurrentContexts = defaultArg minimumConcurrentContexts 3
        let maximumConcurrentContexts = defaultArg maximumConcurrentContexts 20
        if minimumConcurrentContexts < 0 then invalidArg "minimumConcurrentContexts" "Must be non-negative."
        if maximumConcurrentContexts < minimumConcurrentContexts then invalidArg "minimumConcurrentContexts" "should be greater or equal to 'minimumConcurrentContexts'."
        if maxTasksPerContext |> Option.exists (fun mtpd -> mtpd <= 0) then invalidArg "maxTasksPerContext" "should be positive."
        new AssemblyLoadContextPool<'Manager>(minimumConcurrentContexts, maximumConcurrentContexts, configuration, threshold, maxTasksPerContext)

//
//  Lambda Evaluator AssemblyLoadContext pool
//

[<AutoOpen>]
module private EvaluatorImpl =

    /// AssemblyLoadContext configuration that performs side-effectful initialization
    type LoadContextEvaluationConfig = unit -> unit

    /// LoadContext manager object
    type LoadContextEvaluationManager() =
        let mutable taskCount = 0
        let mutable lastUsed = DateTime.Now

        let init () =
            let _ = Interlocked.Increment &taskCount
            lastUsed <- DateTime.Now

        let fini () =
            let _ = Interlocked.Decrement &taskCount
            lastUsed <- DateTime.Now

        /// Synchronously executes a computation in remote LoadContext
        member __.EvaluateSync(func : unit -> 'T) =
            init ()
            try func () finally fini ()

        /// Asynchronously executes a computation in remote LoadContext
        member __.EvaluateAsync(func : Async<'T>) : Async<'T> = async {
            init ()
            try return! func finally fini ()
        }

        interface ILoadContextManager with
            member e.Initialize(config : obj) =
                match config with
                | :? LoadContextEvaluationConfig as c -> c ()
                | _ -> ()

            member e.TaskCount = taskCount
            member e.LastUsed = lastUsed
            member e.Dispose() = ()

/// Defines an AssemblyLoadContext pool that evaluates code based on Vagabond dependency affinities
[<AutoSerializable(false)>]
type AssemblyLoadContextEvaluatorPool private (pool : AssemblyLoadContextPool<LoadContextEvaluationManager>) =

    /// <summary>
    ///     Creates a new AssemblyLoadContextEvaluatorPool instance.
    /// </summary>
    /// <param name="loadContextInitializer">Initializer run on every AssemblyLoadContext upon creation. Defaults to none.</param>
    /// <param name="minimumConcurrentContexts">Minimum allowed AssemblyLoadContexts. Defaults to 3.</param>
    /// <param name="maximumConcurrentContexts">Maximum allowed AssemblyLoadContexts. Defaults to 20.</param>
    /// <param name="threshold">TimeSpan after which unused AssemblyLoadContexts may get unloaded. Defaults to infinite.</param>
    /// <param name="maxTasksPerContext">Maximum number of tasks allowed per AssemblyLoadContext. Defaults to infinite.</param>
    static member Create(?loadContextInitializer : unit -> unit, ?minimumConcurrentContexts : int, ?maximumConcurrentContexts : int, ?threshold : TimeSpan, ?maxTasksPerContext : int) =
        let config = defaultArg loadContextInitializer id
        let pool = AssemblyLoadContextPool.Create<LoadContextEvaluationManager>(config, ?threshold = threshold, ?maxTasksPerContext = maxTasksPerContext,
                            ?minimumConcurrentContexts = minimumConcurrentContexts, ?maximumConcurrentContexts = maximumConcurrentContexts)

        new AssemblyLoadContextEvaluatorPool(pool)

    /// Current AssemblyLoadContext count
    member __.LoadContextCount = pool.LoadContextCount
    /// Maximum allowed AssemblyLoadContext count
    member __.MaxLoadContexts = pool.MaxLoadContexts
    /// Minimum allowed AssemblyLoadContext count
    member __.MinLoadContexts = pool.MinLoadContexts
    
    /// <summary>
    ///     Evaluates function in pooled AssemblyLoadContext. LoadContext will be allocated based on dependency affinity.
    /// </summary>
    /// <param name="dependencies">Dependencies for operation.</param>
    /// <param name="f">Function to be executed.</param>
    member __.Evaluate(dependencies : seq<AssemblyId>, f : unit -> 'T) : 'T =
        let proxy = pool.RequestLoadContext dependencies
        proxy.Execute (fun evaluator -> evaluator.EvaluateSync f)

    /// <summary>
    ///     Asynchronously evaluates function in pooled LoadContext. AssemblyLoadContext will be selected based on dependency affinity.
    /// </summary>
    /// <param name="dependencies">Dependencies for operation.</param>
    /// <param name="f">Asynchronous workflow to be executed.</param>
    member __.EvaluateAsync(dependencies : seq<AssemblyId>, f : Async<'T>) : Async<'T> = async {
        let proxy = pool.RequestLoadContext dependencies
        return! proxy.ExecuteAsync (fun evaluator -> evaluator.EvaluateAsync f)
    }

    interface IDisposable with
        member __.Dispose () = (pool :> IDisposable).Dispose()