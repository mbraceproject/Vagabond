namespace MBrace.Vagabond.AppDomainPool

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization
open System.Runtime.Loader
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
    inherit IDisposable
    /// AppDomain initializer method.
    abstract Initialize : IAppDomainConfiguration -> unit
    /// Gets the number of tasks currently active in the AppDomain. 
    /// AppDomains with non-zero task count will never be unloaded
    /// by the AppDomainPool.
    abstract TaskCount : int
    /// Gets the last time this app domain instance was used.
    /// Used by the AppDomainPool to unload unused instances.
    abstract LastUsed : DateTime

/// Proxy interface used for communicating with type loaded in remote AssemblyLoadContext
type ILoadContextProxy<'T when 'T :> IDisposable> =
    inherit IDisposable
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

        static member CreateProxyFromMarshallerHandle (remoteHandle : obj) =
            let remoteMethod = remoteHandle.GetType().GetMethod("ExecuteMarshalled", BindingFlags.NonPublic ||| BindingFlags.Instance)
            let remoteAsyncMethod = remoteHandle.GetType().GetMethod("ExecuteMarshalledAsync", BindingFlags.NonPublic ||| BindingFlags.Instance)
            { new ILoadContextProxy<'T> with
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
            LoadContextMarshaller<'T>.CreateProxyFromMarshallerHandle remoteInstance

    /// An assembly load context that mirrors assembly loading from the currently running context
    type MirroredAssemblyLoadContext() =
        inherit AssemblyLoadContext() // TODO need the isCollectible=true constructor overload here

        // AssemblyLoadContext is not available in netstandard2.0
        // Use reflection to access its APIs
        let unloadMethod = typeof<AssemblyLoadContext>.GetMethod("Unload", BindingFlags.Public ||| BindingFlags.Instance)
        let getLoadedAssemblies = Utils.getLoadedAssemblies.Value

        let tryResolveFileName (an : AssemblyName) =
            let isMatchingAssembly (assembly : Assembly) =
                let can = assembly.GetName()
                can.Name = an.Name &&
                can.Version >= an.Version &&
                can.GetPublicKeyToken() = an.GetPublicKeyToken()

            getLoadedAssemblies()
            |> Seq.filter (fun a -> not a.IsDynamic && not (String.IsNullOrEmpty a.Location))
            |> Seq.tryFind isMatchingAssembly
            |> Option.map (fun a -> a.Location)

        /// Due to nestandard constraints, need to call "Unload" method using reflection
        member this.TryUnload() = 
            match unloadMethod with
            | null -> false
            | m -> m.Invoke(this, [||]) |> ignore ; true
                
        override this.Load an =
            match tryResolveFileName an with
            | None -> null
            | Some path -> this.LoadFromAssemblyPath path

    /// Application Domain load state
    type AppDomainLoadInfo<'Manager when 'Manager :> IAppDomainManager
                                     and 'Manager : (new : unit -> 'Manager)> =
        {
            /// AppDomain identifier
            Id : string
            /// AppDomain instance
            LoadContext : MirroredAssemblyLoadContext
            /// AppDomain manager instance
            Proxy : ILoadContextProxy<'Manager>
            /// Declared Vagabond dependency load state
            Dependencies : Map<string, AssemblyId>
        }
    with
        /// Initialize an AppDomain instance with given Vagabond dependencies
        static member Init (config : IAppDomainConfiguration, dependencies : seq<AssemblyId>) =
            let id = Guid.NewGuid().ToString()
            let dependencies = dependencies |> Seq.map (fun d -> d.FullName, d) |> Map.ofSeq
            let loadContext = new MirroredAssemblyLoadContext()
            let proxy = loadContext.CreateProxy<'Manager>()
            proxy.Execute(fun m -> m.Initialize config)

            { Id = id; Proxy = proxy; LoadContext = loadContext; Dependencies = dependencies }

        member adli.AddDependencies (dependencies : seq<AssemblyId>) =
            { adli with Dependencies = dependencies |> Seq.map (fun d -> d.FullName, d) |> Map.addMany adli.Dependencies }


    /// Globad AppDomain load state
    type AppDomainPoolInfo<'Manager when 'Manager :> IAppDomainManager
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
            /// User-specified configuration for the AppDomain
            Configuration : IAppDomainConfiguration
            /// Unload threshold: minimum timespan needed for unloading unused domains
            UnloadThreshold : TimeSpan option
        }
    with
        /// Initialize a new AppDomain with provided dependencies and add to state
        member s.AddNew(dependencies) =
            let adli = AppDomainLoadInfo<'Manager>.Init (s.Configuration, dependencies = dependencies)
            adli, s.AddDomain adli

        /// Add existing or updated AppDomain info to state
        member s.AddDomain (adli : AppDomainLoadInfo<'Manager>) =
            { s with DomainPool = s.DomainPool.Add(adli.Id, adli) }

        /// Initialize a new AppDomain pool state
        static member Init(minimumConcurrentDomains : int, maximumConcurrentDomains : int, config : IAppDomainConfiguration ,threshold : TimeSpan option, maxTasks : int option) =
            {
                DomainPool = Map.empty
                MaxConcurrentDomains = maximumConcurrentDomains
                MinConcurrentDomains = minimumConcurrentDomains
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
                if state.MaxTasksPerDomain |> Option.exists (fun mtpd -> adli.Proxy.Execute(fun m -> m.TaskCount) >= mtpd) 
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
                try adli.Proxy.Execute(fun m -> m.Dispose())
                finally adli.LoadContext.TryUnload() |> ignore
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
            |> Seq.filter(fun adli -> adli.Proxy.Execute(fun m -> m.TaskCount = 0))
            |> Seq.sortBy(fun adli -> adli.Dependencies.Count)

        for adli in unusedDomains do
            if pool.Count <= state.MinConcurrentDomains then ()
            elif removed.Count < minRemovals || state.UnloadThreshold |> Option.exists(fun th -> now - adli.Proxy.Execute(fun m -> m.LastUsed) > th) then
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
                                     and 'Manager : (new : unit -> 'Manager)> =
        | GetDomain of dependencies : AssemblyId [] * ReplyChannel<ILoadContextProxy<'Manager>>
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
                | Success(state2, adli) -> rc.Reply adli.Proxy ; return! behaviour (Some state2) self
                | Error e -> rc.ReplyWithError e ; return! behaviour gstate self

            | GetState rc -> rc.Reply state ; return! behaviour gstate self
            | Dispose rc ->
                state.DomainPool |> Seq.map (function KeyValue(_,adli) -> adli) |> disposeDomains |> Async.Start
                rc.Reply (())
                return! behaviour None self

            | Cleanup ->
                match Exn.protect2 cleanupDomains 0 state with
                | Success state2 -> return! behaviour (Some state2) self
                | Error _ -> return! behaviour gstate self
    }

/// Provides an AppDomain pooling mechanism for use by Vagabond.
/// AppDomains are managed based on what assembly dependencies are required for execution.
[<AutoSerializable(false)>]
type AppDomainPool<'Manager when 'Manager :> IAppDomainManager
                             and 'Manager : (new : unit -> 'Manager)>

    internal (minimumConcurrentDomains : int, maximumConcurrentDomains : int, config : IAppDomainConfiguration, threshold : TimeSpan option, maxTasks : int option) =

    let cts = new CancellationTokenSource()
    let state = AppDomainPoolInfo<'Manager>.Init(minimumConcurrentDomains, maximumConcurrentDomains, config, threshold, maxTasks)
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
                try mbox.Post AppDomainPoolMsg<'Manager>.Cleanup with _ -> ()
                return! collector ()
            }

            do Async.Start(collector (), cts.Token)

    /// <summary>
    ///     Returns a marshalled Manager object attached to a pooled AppDomain instance.
    ///     AppDomain will be selected based on dependency affinity.
    /// </summary>
    /// <param name="dependencies">Assembly dependencies required of AppDomain.</param>
    member __.RequestAppDomain(dependencies : seq<AssemblyId>) : ILoadContextProxy<'Manager> =
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
        |> Seq.map (function KeyValue(_,v) -> v.LoadContext :> AssemblyLoadContext, v.Proxy)
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
    static member Create<'Manager when 'Manager :> IAppDomainManager
                                   and 'Manager : (new : unit -> 'Manager)>
        (configuration : IAppDomainConfiguration, ?minimumConcurrentDomains : int, ?maximumConcurrentDomains : int, ?threshold : TimeSpan, ?maxTasksPerDomain : int) =

        let minimumConcurrentDomains = defaultArg minimumConcurrentDomains 3
        let maximumConcurrentDomains = defaultArg maximumConcurrentDomains 20
        if minimumConcurrentDomains < 0 then invalidArg "minimumConcurrentDomains" "Must be non-negative."
        if maximumConcurrentDomains < minimumConcurrentDomains then invalidArg "minimumConcurrentDomains" "should be greater or equal to 'minimumConcurrentDomains'."
        if maxTasksPerDomain |> Option.exists (fun mtpd -> mtpd <= 0) then invalidArg "maxTasksPerDomain" "should be positive."
        new AppDomainPool<'Manager>(minimumConcurrentDomains, maximumConcurrentDomains, configuration, threshold, maxTasksPerDomain)


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
                                 and 'Manager : (new : unit -> 'Manager)
                                 and 'Config :> IAppDomainConfiguration
                                 and 'Config : (new : unit -> 'Config)>
        (?minimumConcurrentDomains : int, ?maximumConcurrentDomains : int, ?threshold : TimeSpan, ?maxTasksPerDomain : int) =

        AppDomainPool.Create<'Manager>(activate typeof<'Config> :?> 'Config, 
            ?minimumConcurrentDomains = minimumConcurrentDomains, ?maximumConcurrentDomains = maximumConcurrentDomains, 
            ?threshold = threshold, ?maxTasksPerDomain = maxTasksPerDomain)

//
//  Lambda Evaluator AppDomain pool: application of the AppDomain pool that uses FsPickler for sending lambdas 
//  or asynchronous workflows for execution in pooled AppDomains
//

[<AutoOpen>]
module private EvaluatorImpl =

    /// AppDomain configuration that carries a pickled initializer
    type AppDomainEvaluatorConfiguration(?initializer : unit -> unit) =
        let initializer = defaultArg initializer id
        member __.Initializer = initializer
        interface IAppDomainConfiguration

    /// AppDomain managing object
    type AppDomainEvaluatorManager() =
        let mutable taskCount = 0
        let mutable lastUsed = DateTime.Now

        let init () =
            let _ = Interlocked.Increment &taskCount
            lastUsed <- DateTime.Now

        let fini () =
            let _ = Interlocked.Decrement &taskCount
            lastUsed <- DateTime.Now

        /// Synchronously executes a computation in remote AppDomain
        member __.EvaluateSync(func : unit -> 'T) =
            init ()
            try func () finally fini ()

        /// Asynchronously executes a computation in remote AppDomain
        member __.EvaluateAsync(func : Async<'T>) : Async<'T> = async {
            init ()
            try return! func finally fini ()
        }

        interface IAppDomainManager with
            member e.Initialize(config : IAppDomainConfiguration) =
                match config with
                | :? AppDomainEvaluatorConfiguration as c -> c.Initializer()
                | _ -> ()

            member e.TaskCount = taskCount
            member e.LastUsed = lastUsed
            member e.Dispose() = ()

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
    static member Create(?appDomainInitializer : unit -> unit, ?minimumConcurrentDomains : int, ?maximumConcurrentDomains : int, ?threshold : TimeSpan, ?maxTasksPerDomain : int) =
        let config = new AppDomainEvaluatorConfiguration(?initializer = appDomainInitializer)
        let pool = AppDomainPool.Create<AppDomainEvaluatorManager>(config, ?threshold = threshold, ?maxTasksPerDomain = maxTasksPerDomain,
                            ?minimumConcurrentDomains = minimumConcurrentDomains, ?maximumConcurrentDomains = maximumConcurrentDomains)

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
        let proxy = pool.RequestAppDomain dependencies
        proxy.Execute (fun evaluator -> evaluator.EvaluateSync f)

    /// <summary>
    ///     Asynchronously evaluates function in pooled AppDomain. AppDomain will be selected based on dependency affinity.
    /// </summary>
    /// <param name="dependencies">Dependencies for operation.</param>
    /// <param name="f">Asynchronous workflow to be executed.</param>
    member __.EvaluateAsync(dependencies : seq<AssemblyId>, f : Async<'T>) : Async<'T> = async {
        let proxy = pool.RequestAppDomain dependencies
        return! proxy.ExecuteAsync (fun evaluator -> evaluator.EvaluateAsync f)
    }

    interface IDisposable with
        member __.Dispose () = (pool :> IDisposable).Dispose()