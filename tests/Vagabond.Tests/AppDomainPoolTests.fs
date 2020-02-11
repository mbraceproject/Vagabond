namespace MBrace.Vagabond.Tests

open System
open System.Reflection
open System.Runtime.Loader
open System.Threading

open Xunit
open MBrace.FsPickler
open MBrace.Vagabond
open MBrace.Vagabond.AppDomainPool

[<TestCaseOrderer("MBrace.Vagabond.Tests.Utils.AlphabeticalOrderer", "Vagabond.Tests")>]
module ``AppDomain Pool Tests`` =

    let minDomains = 3
    let maxDomains = 20

    type AppDomainPoolTester () =

        let id = Guid.NewGuid()
        let mutable taskCount = 0
        let mutable isInitialized = false
        let mutable isFinalized = false
        let mutable lastUsed = DateTime.Now

        member __.Id = id
        member __.IsInitialized = isInitialized
        member __.IsFinalized = isFinalized
        member __.Reset () = taskCount <- 0

        member __.AddTask () = 
            let _ = Interlocked.Increment &taskCount
            lastUsed <- DateTime.Now

        member __.RemoveTask () =
            let _ = Interlocked.Decrement &taskCount
            lastUsed <- DateTime.Now

        interface IAppDomainManager with
            member __.Initialize (_ : _) = isInitialized <- true
            member __.Dispose() = isFinalized <- true
            member __.TaskCount = taskCount
            member __.LastUsed = lastUsed

        static member Init(?threshold, ?maxTasks) = 
            AppDomainPool.Create<AppDomainPoolTester>(Unchecked.defaultof<_>, minDomains, maxDomains, ?threshold = threshold, ?maxTasksPerDomain = maxTasks)

    let (!) (client : AppDomainPoolTester) = client :> IAppDomainManager

    let idOf<'T> = Vagabond.ComputeAssemblyId typeof<'T>.Assembly

    let idOfWith<'T> i = { idOf<'T> with ImageHash = [|i|] }

    let getLoadContextId() = AssemblyLoadContext.GetLoadContext(Assembly.GetExecutingAssembly()).Name

    [<Fact>]
    let ``01 Simple init and tear down an application domain.`` () =
        use pool = AppDomainPoolTester.Init()
        let dependencies = [ idOf<int> ; idOf<int option> ]
        let proxy = pool.RequestAppDomain dependencies
        proxy.Execute(fun mgr ->
            mgr.IsInitialized |> shouldEqual true
            mgr.IsFinalized |> shouldEqual false
            (! mgr).TaskCount |> shouldEqual 0
            mgr.AddTask ()
            (! mgr).TaskCount |> shouldEqual 1
            mgr.RemoveTask ()
            (! mgr).TaskCount |> shouldEqual 0)

    [<Fact>]
    let ``02A Empty dependency domain.`` () =
        use pool = AppDomainPoolTester.Init()
        let proxy = pool.RequestAppDomain []
        proxy.Execute(fun mgr ->
            mgr.AddTask () 
            (! mgr).TaskCount |> shouldEqual 1
            mgr.RemoveTask ()
            (! mgr).TaskCount |> shouldEqual 0)

    [<Fact>]
    let ``02B Concurrent appdomain requests.`` () =
        use pool = AppDomainPoolTester.Init()
        let dependencies = [ idOf<int> ; idOf<int option> ]
        let proxies = Async.RunSynchronously(async {
            let wf = async { return pool.RequestAppDomain dependencies }
            return! Seq.init 10 (fun _ -> wf) |> Async.Parallel
        })

        proxies |> Seq.distinctBy (fun p -> p.Execute(fun m -> m.Id)) |> Seq.length |> shouldEqual 1

    [<Fact>]
    let ``03 Should use the same AppDomain when using identical dependency sets.`` () =
        use pool = AppDomainPoolTester.Init()
        let dependencies = [ idOf<int> ; idOf<int option> ]
        let proxy = pool.RequestAppDomain dependencies
        let id = proxy.Execute(fun m -> m.Id)
        for i = 1 to 10 do
            let proxy' = pool.RequestAppDomain dependencies
            let id' = proxy'.Execute(fun m -> m.Id)
            id' |> shouldEqual id

    [<Fact>]
    let ``04 Should use the same AppDomain when extending the dependency set.`` () =
        use pool = AppDomainPoolTester.Init()
        let dependencies = [ idOf<int> ]
        let dependencies' = idOf<int option> :: dependencies
        let proxy = pool.RequestAppDomain dependencies
        let id = proxy.Execute(fun m -> m.Id)
        for i = 1 to 10 do
            let proxy' = pool.RequestAppDomain dependencies'
            let id' = proxy'.Execute(fun m -> m.Id)
            id' |> shouldEqual id

    [<Fact>]
    let ``05 Should use separate AppDomain when using incompatible dependency sets.`` () =
        use pool = AppDomainPoolTester.Init()
        let dependencies = [ idOf<int> ; idOf<int option> ]
        let proxy = pool.RequestAppDomain dependencies
        let id = proxy.Execute(fun m -> m.Id)
        let dependencies' = [ idOf<int> ; { idOf<int option> with ImageHash = [||] } ]
        for i = 1 to 10 do
            let proxy' = pool.RequestAppDomain dependencies'
            let id' = proxy'.Execute(fun m -> m.Id)
            id' |> shouldNotEqual id

    [<Fact>]
    let ``06 Should create new AppDomains when conflicting dependencies.`` () =
        use pool = AppDomainPoolTester.Init()
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] ; Extension = ".dll" }]
        Seq.init maxDomains (fun i -> let proxy = pool.RequestAppDomain (mkDeps i) in proxy.Execute(fun m -> m.Id))
        |> Seq.distinct
        |> Seq.length
        |> shouldEqual maxDomains

    [<Fact>]
    let ``07 Should fail when passing max domain threshold.`` () =
        use pool = AppDomainPoolTester.Init()
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] ; Extension = ".dll" }]
        shouldFailwith<_,OutOfResourcesException>(fun () ->
            for i in 1 .. maxDomains + 1 do
                let proxy = pool.RequestAppDomain (mkDeps i)
                proxy.Execute(fun m -> m.Reset())
                proxy.Execute(fun m -> m.AddTask()))

    [<Fact>]
    let ``08A Should successfully dispose idle domains when passing domain threshold.`` () =
        use pool = AppDomainPoolTester.Init()
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] ; Extension = ".dll" }]
        for i in 1 .. 3 * maxDomains do
            let proxy = pool.RequestAppDomain (mkDeps i)
            proxy.Execute(fun m -> m.Reset())

    [<Fact>]
    let ``08B Domain count should contract to minimum count after threshold passed.`` () =
        use manager = AppDomainPoolTester.Init(threshold = TimeSpan.FromMilliseconds 1.)
        for i in 1 .. 3 * minDomains do
            let _ = manager.RequestAppDomain [idOfWith<int> (byte i)]
            ()

        do Thread.Sleep 1000
        manager.DomainCount |> shouldBe (fun c -> c = minDomains)

    [<Fact>]
    let ``09 Should automatically dispose instances in pool with timespan threshold.`` () =
        use pool = AppDomainPoolTester.Init(threshold = TimeSpan.FromSeconds 1.)
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] ; Extension = ".dll" }]
        for i in 1 .. maxDomains do
            let mgr = pool.RequestAppDomain (mkDeps i) in ()

        do Thread.Sleep 2000

        pool.DomainCount |> shouldEqual pool.MinDomains

    [<Fact>]
    let ``10 Should not automatically dispose busy instances in pool with timespan threshold.`` () =
        use pool = AppDomainPoolTester.Init(threshold = TimeSpan.FromSeconds 1.)
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] ; Extension = ".dll" }]
        for i in 1 .. maxDomains do
            let proxy = pool.RequestAppDomain (mkDeps i) in ()
            proxy.Execute(fun m -> m.AddTask())

        do Thread.Sleep 2000

        pool.DomainCount |> shouldEqual pool.MaxDomains

    [<Fact>]
    let ``11 Should not return the same AppDomain when task threshold has been reached.`` () =
        use pool = AppDomainPoolTester.Init(maxTasks = 1)
        Seq.init maxDomains (fun i -> 
            let proxy = pool.RequestAppDomain [ idOf<int> ]
            proxy.Execute(fun m -> m.AddTask() ; m.Id))
        |> Seq.distinct
        |> Seq.length
        |> shouldEqual maxDomains


    [<Fact>]
    let ``12 AppDomainEvaluatorPool simple lambda`` () =
        use pool = AppDomainEvaluatorPool.Create(fun () -> printfn "Initializing AppDomain")
        pool.Evaluate([], fun () -> 1 + 1) |> shouldEqual 2

    [<Fact>]
    let ``13 AppDomainEvaluatorPool simple worfklow`` () =
        use pool = AppDomainEvaluatorPool.Create(fun () -> printfn "Initializing AppDomain")
        pool.EvaluateAsync([], async { return getLoadContextId() }) 
        |> Async.RunSynchronously 
        |> shouldNotEqual (getLoadContextId())

    [<Fact>]
    let ``14 AppDomainEvaluatorPool simple worfklow with exception`` () =
        use pool = AppDomainEvaluatorPool.Create(fun () -> printfn "Initializing AppDomain")
        Assert.Throws<System.InvalidOperationException>(fun () -> pool.EvaluateAsync([], async { return invalidOp "boom"}) |> Async.RunSynchronously |> ignore) 
        |> ignore


    type AppDomainVagabondLambdaLoaderConfiguration() =
        let cachePath = VagabondConfig.Init().CachePath
        interface IAppDomainConfiguration
        member __.CachePath = cachePath

    type AppDomainVagabondLambdaLoader () =
        let mutable lastUsed = DateTime.Now
        let mutable taskCount = 0

        member __.Evaluate(dependencies : VagabondAssembly [], plambda : Pickle<unit -> 'T>) : Pickle<Choice<'T, exn>> =
            let _ = VagabondConfig.Instance.LoadVagabondAssemblies dependencies
            let lambda = VagabondConfig.Instance.Serializer.UnPickleTyped plambda
            let _ = Interlocked.Increment &taskCount
            let result = try lambda () |> Choice1Of2 with e -> Choice2Of2 e
            let _ = Interlocked.Decrement &taskCount
            lastUsed <- DateTime.Now
            VagabondConfig.Instance.Serializer.PickleTyped result         
        
        interface IAppDomainManager with
            member __.Initialize (config : IAppDomainConfiguration) =
                match config with
                | :? AppDomainVagabondLambdaLoaderConfiguration as c -> VagabondConfig.Init(c.CachePath) |> ignore
                | _ -> ()

            member __.Dispose () = ()
            member __.LastUsed = lastUsed
            member __.TaskCount = taskCount


        static member Eval (vpm : AppDomainPool<AppDomainVagabondLambdaLoader>) (f : unit -> 'T) =
            let vg = VagabondConfig.Instance
            let deps = vg.ComputeObjectDependencies(f, true)
            let proxy = vpm.RequestAppDomain(deps |> Seq.map (fun deps -> deps.Id))
            let p = VagabondConfig.Pickler.PickleTyped f
            let presult = proxy.Execute(fun m -> m.Evaluate(deps, p))
            match vg.Serializer.UnPickleTyped presult with
            | Choice1Of2 v -> v
            | Choice2Of2 e -> raise e

        static member Init() = AppDomainPool.Create<AppDomainVagabondLambdaLoader, AppDomainVagabondLambdaLoaderConfiguration> ()

    [<Fact>]
    let ``15 AppDomain Vagabond Lambda Evaluator`` () =
        use pool = AppDomainVagabondLambdaLoader.Init()
        AppDomainVagabondLambdaLoader.Eval pool (fun () -> 1 + 1) |> shouldEqual 2


    type StaticValueContainer private () =
        static let id = Guid.NewGuid().ToString()
        static member Value = id

    [<Fact>]
    let ``16 AppDomain Vagabond Lambda Evaluator should always use the same domain`` () =
        use pool = AppDomainVagabondLambdaLoader.Init()
        Seq.init 10 (fun _ -> AppDomainVagabondLambdaLoader.Eval pool (fun () -> StaticValueContainer.Value))
        |> Seq.distinct
        |> Seq.length
        |> shouldEqual 1