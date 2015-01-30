namespace Nessos.Vagabond.AppDomainIsolation

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization
open System.Threading
open System.Threading.Tasks

open Microsoft.FSharp.Control

open Nessos.FsPickler
open Nessos.Vagabond

type IAppDomainClient =
    abstract Init : unit -> unit
    abstract Fini : unit -> unit
    abstract TaskCount : int
    abstract LastUsed : DateTime

/// Marshalled task completion source for async computation over AppDomains
type MarshalledTaskCompletionSource<'T> () =
    inherit MarshalByRefObject ()
    let tcs = new TaskCompletionSource<'T> ()
    member __.SetResult(t : 'T) = tcs.SetResult t
    member __.SetException(e : exn) = tcs.SetException e
    member __.SetCanceled() = tcs.SetCanceled()
    member __.Task = tcs.Task

type MarshalledCancellationTokenSource () =
    inherit MarshalByRefObject ()
    let cts = new CancellationTokenSource()
    member __.Cancel() = cts.Cancel()
    member __.CancellationToken = cts.Token

[<AutoOpen>]
module private Impl =

    let initAppDomain (name : string) =
        let currentDomain = AppDomain.CurrentDomain
        let appDomainSetup = currentDomain.SetupInformation
        let evidence = new Security.Policy.Evidence(currentDomain.Evidence)
        AppDomain.CreateDomain(name, evidence, appDomainSetup)

    let initAppDomainObject<'T when 'T :> MarshalByRefObject and 'T : (new : unit -> 'T)> (targetDomain : AppDomain) =
        let assemblyName = typeof<'T>.Assembly.FullName
        let typeName = typeof<'T>.FullName
        let culture = System.Globalization.CultureInfo.CurrentCulture
        let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance
        let handle = targetDomain.CreateInstance(assemblyName, typeName, false, flags, null, [||], culture, [||])
        handle.Unwrap() :?> 'T

    type AppDomainLoadInfo<'Client when 'Client :> IAppDomainClient 
                                   and 'Client :> MarshalByRefObject 
                                   and 'Client : (new : unit -> 'Client)> =
        {
            Id : string
            AppDomain : AppDomain
            Client : 'Client
            Dependencies : Map<string, AssemblyId>
        }
    with
        static member Init (?dependencies : seq<AssemblyId>) =
            let id = Guid.NewGuid().ToString()
            let dependencies = 
                match dependencies with
                | None -> Map.empty
                | Some ds -> ds |> Seq.map (fun d -> d.FullName, d) |> Map.ofSeq

            let appDomain = initAppDomain id
            let client = initAppDomainObject<'Client> appDomain
            do client.Init ()

            { Id = id; Client = client; AppDomain = appDomain; Dependencies = dependencies }

        member adli.AddDependencies (dependencies : seq<AssemblyId>) =
            { adli with Dependencies = Map.addMany (dependencies |> Seq.map (fun d -> d.FullName, d)) adli.Dependencies }

    type AppDomainIsolationState<'Client when 'Client :> IAppDomainClient 
                                         and 'Client :> MarshalByRefObject 
                                         and 'Client : (new : unit -> 'Client)> =
        {
            DomainPool : Map<string, AppDomainLoadInfo<'Client>>
            MaxConcurrentDomains : int
            MinConcurrentDomains : int
            UnloadThreshold : TimeSpan option
        }
    with
        member s.AddNew(?dependencies) =
            let adli = AppDomainLoadInfo<'Client>.Init (?dependencies = dependencies)
            adli, s.AddDomain adli

        member s.AddDomain (adli : AppDomainLoadInfo<'Client>) =
            { s with DomainPool = s.DomainPool.Add(adli.Id, adli) }

        static member Init(minimumConcurrentDomains : int, maximumConcurrentDomains : int, threshold : TimeSpan option) =
            let mutable empty = {
                DomainPool = Map.empty
                MaxConcurrentDomains = maximumConcurrentDomains
                MinConcurrentDomains = minimumConcurrentDomains
                UnloadThreshold = threshold
            }

            for _ in 1 .. minimumConcurrentDomains do
                empty <- empty.AddNew() |> snd

            empty

    let tryGetMatchingAppDomain (state : AppDomainIsolationState<'C>) (dependencies : AssemblyId []) : AppDomainLoadInfo<'C> option * AppDomainIsolationState<'C> =
        let getCompatibility (adli : AppDomainLoadInfo<'C>) =
            let missingCount = ref 0
            let isCompatibleAssembly (id : AssemblyId) = 
                match adli.Dependencies.TryFind id.FullName with
                | None -> incr missingCount ; true
                | Some id' -> id = id'

            if Array.forall isCompatibleAssembly dependencies then Some (adli, !missingCount)
            else None

        let compatibleAppDomains = state.DomainPool |> Seq.choose(function (KeyValue(_,adli)) -> getCompatibility adli) |> Seq.toArray
        match compatibleAppDomains with
        | [||] -> None, state
        | _ -> 
            // locate best compatible AppDomain
            let adli = compatibleAppDomains |> Array.minBy snd |> fst
            // append dependencies to load state
            let adli2 = adli.AddDependencies dependencies
            let state2 = state.AddDomain adli2
            Some adli2, state2


    let disposeDomains (domains : seq<AppDomainLoadInfo<'C>>) = async {
        for adli in domains do
            try 
                try adli.Client.Fini()
                finally AppDomain.Unload adli.AppDomain 
            with _ -> ()
    }

    let cleanupDomains (minRemovals : int) (state : AppDomainIsolationState<'C>) : AppDomainIsolationState<'C> =
        let removed = new ResizeArray<AppDomainLoadInfo<'C>> ()
        let mutable pool = state.DomainPool
        let now = DateTime.Now

        let unusedDomains =         
            state.DomainPool
            |> Seq.map(function KeyValue(_,adli) -> adli)
            |> Seq.filter(fun kv -> kv.Client.TaskCount = 0)

        for adli in unusedDomains do
            if pool.Count <= state.MinConcurrentDomains then ()
            elif removed.Count < minRemovals || state.UnloadThreshold |> Option.exists(fun th -> now - adli.Client.LastUsed > th) then
                removed.Add adli
                pool <- pool.Remove adli.Id

        if removed.Count < minRemovals then
            let msg = sprintf "AppDomain threshold of %d instances has been reached." state.MaxConcurrentDomains
            raise <| new OutOfResourcesException(msg)

        Async.Start(disposeDomains removed)

        { state with DomainPool = pool }

    let getMatchingAppDomain (state : AppDomainIsolationState<'C>) (dependencies : AssemblyId []) =
        match tryGetMatchingAppDomain state dependencies with
        | Some adli, state  -> state, adli
        | None, state ->
            let excessDomains = state.DomainPool.Count - state.MaxConcurrentDomains + 1
            let state2 =
                if excessDomains > 0 then cleanupDomains excessDomains state
                else state

            let adli, state3 = state2.AddNew(dependencies)
            state3, adli


    type IsolationManagerMsg<'Client when 'Client :> IAppDomainClient 
                                     and 'Client :> MarshalByRefObject 
                                     and 'Client : (new : unit -> 'Client)> =

        | GetDomain of dependencies : AssemblyId [] * ReplyChannel<'Client>
        | GetState of ReplyChannel<AppDomainIsolationState<'Client>>
        | Dispose of ReplyChannel<unit>
        | Cleanup

    let rec behaviour (gstate : AppDomainIsolationState<'C> option) (self : MailboxProcessor<IsolationManagerMsg<'C>>) = async {
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
                | Success(state2, adli) -> rc.Reply adli.Client ; return! behaviour (Some state2) self
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


type AppDomainIsolationManager<'Client when 'Client :> IAppDomainClient 
                                     and 'Client :> MarshalByRefObject 
                                     and 'Client : (new : unit -> 'Client)>

    internal (minimumConcurrentDomains : int, maximumConcurrentDomains : int, threshold : TimeSpan option) =

    let cts = new CancellationTokenSource()
    let state = AppDomainIsolationState<'Client>.Init(minimumConcurrentDomains, maximumConcurrentDomains, threshold)
    let mbox = MailboxProcessor.Start(behaviour (Some state))
    let getState () = mbox.PostAndReply GetState

    do
        match threshold with
        | None -> ()
        | Some ts ->
            let sleepInterval = min (int ts.TotalMilliseconds / 2) 10000
            let rec collector () = async {
                do! Async.Sleep sleepInterval
                try mbox.Post IsolationManagerMsg<'Client>.Cleanup with _ -> ()
                return! collector ()
            }

            do Async.Start(collector (), cts.Token)

    member __.GetAppDomainClient(dependencies : seq<AssemblyId>) : 'Client = 
        mbox.PostAndReply (fun ch -> GetDomain(Seq.toArray dependencies, ch))

    member __.DomainCount = getState().DomainPool.Count
    member __.MaxDomains = getState().MaxConcurrentDomains
    member __.MinDomains = getState().MinConcurrentDomains

#if DEBUG
    member __.State = 
        getState().DomainPool 
        |> Seq.map (function KeyValue(_,v) -> v.Client, v.AppDomain)
        |> Seq.toArray
#endif

    interface IDisposable with
        member __.Dispose () =
            if not cts.IsCancellationRequested then
                do mbox.PostAndReply IsolationManagerMsg<'Client>.Dispose
                cts.Cancel()


type AppDomainIsolationManager =
    static member Create<'Client when 'Client :> IAppDomainClient 
                                 and 'Client :> MarshalByRefObject 
                                 and 'Client : (new : unit -> 'Client)>
        (?minimumConcurrentDomains : int, ?maximumConcurrentDomains : int, ?threshold : TimeSpan) =

        let minimumConcurrentDomains = defaultArg minimumConcurrentDomains 3
        let maximumConcurrentDomains = defaultArg maximumConcurrentDomains 20
        new AppDomainIsolationManager<'Client>(minimumConcurrentDomains, maximumConcurrentDomains, threshold)