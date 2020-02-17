namespace MBrace.Vagabond.Tests

open System
open System.Reflection
open System.Runtime.Loader
open System.Threading

open Xunit
open MBrace.FsPickler
open MBrace.Vagabond
open MBrace.Vagabond.LoadContextPool

[<TestCaseOrderer("MBrace.Vagabond.Tests.Utils.AlphabeticalOrderer", "Vagabond.Tests")>]
module ``AssemblyLoadContextPool Tests`` =

    let minContexts = 3
    let maxContexts = 20

    type AssemblyLoadContextPoolTester () =

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

        interface ILoadContextManager with
            member __.Initialize (_ : _) = isInitialized <- true
            member __.Dispose() = isFinalized <- true
            member __.TaskCount = taskCount
            member __.LastUsed = lastUsed

        static member Init(?threshold, ?maxTasks) = 
            AssemblyLoadContextPool.Create<AssemblyLoadContextPoolTester>(Unchecked.defaultof<_>, minContexts, maxContexts, ?threshold = threshold, ?maxTasksPerContext = maxTasks)

    let (!) (client : AssemblyLoadContextPoolTester) = client :> ILoadContextManager

    let idOf<'T> = Vagabond.ComputeAssemblyId typeof<'T>.Assembly

    let idOfWith<'T> i = { idOf<'T> with ImageHash = [|i|] }

    let getLoadContextId() = AssemblyLoadContext.GetLoadContext(Assembly.GetExecutingAssembly()).Name

    [<Fact>]
    let ``01 Simple init and tear down a load context.`` () =
        use pool = AssemblyLoadContextPoolTester.Init()
        let dependencies = [ idOf<int> ; idOf<int option> ]
        let proxy = pool.RequestLoadContext dependencies
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
        use pool = AssemblyLoadContextPoolTester.Init()
        let proxy = pool.RequestLoadContext []
        proxy.Execute(fun mgr ->
            mgr.AddTask () 
            (! mgr).TaskCount |> shouldEqual 1
            mgr.RemoveTask ()
            (! mgr).TaskCount |> shouldEqual 0)

    [<Fact>]
    let ``02B Concurrent appdomain requests.`` () =
        use pool = AssemblyLoadContextPoolTester.Init()
        let dependencies = [ idOf<int> ; idOf<int option> ]
        let proxies = Async.RunSynchronously(async {
            let wf = async { return pool.RequestLoadContext dependencies }
            return! Seq.init 10 (fun _ -> wf) |> Async.Parallel
        })

        proxies |> Seq.distinctBy (fun p -> p.Execute(fun m -> m.Id)) |> Seq.length |> shouldEqual 1

    [<Fact>]
    let ``03 Should allocate the same LoadContext when using identical dependency sets.`` () =
        use pool = AssemblyLoadContextPoolTester.Init()
        let dependencies = [ idOf<int> ; idOf<int option> ]
        let proxy = pool.RequestLoadContext dependencies
        let id = proxy.Execute(fun m -> m.Id)
        for i = 1 to 10 do
            let proxy' = pool.RequestLoadContext dependencies
            let id' = proxy'.Execute(fun m -> m.Id)
            id' |> shouldEqual id

    [<Fact>]
    let ``04 Should use the same LoadContext when extending the dependency set.`` () =
        use pool = AssemblyLoadContextPoolTester.Init()
        let dependencies = [ idOf<int> ]
        let dependencies' = idOf<int option> :: dependencies
        let proxy = pool.RequestLoadContext dependencies
        let id = proxy.Execute(fun m -> m.Id)
        for i = 1 to 10 do
            let proxy' = pool.RequestLoadContext dependencies'
            let id' = proxy'.Execute(fun m -> m.Id)
            id' |> shouldEqual id

    [<Fact>]
    let ``05 Should use separate LoadContext when using incompatible dependency sets.`` () =
        use pool = AssemblyLoadContextPoolTester.Init()
        let dependencies = [ idOf<int> ; idOf<int option> ]
        let proxy = pool.RequestLoadContext dependencies
        let id = proxy.Execute(fun m -> m.Id)
        let dependencies' = [ idOf<int> ; { idOf<int option> with ImageHash = [||] } ]
        for i = 1 to 10 do
            let proxy' = pool.RequestLoadContext dependencies'
            let id' = proxy'.Execute(fun m -> m.Id)
            id' |> shouldNotEqual id

    [<Fact>]
    let ``06 Should create new LoadContexts when conflicting dependencies.`` () =
        use pool = AssemblyLoadContextPoolTester.Init()
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] ; Extension = ".dll" }]
        Seq.init maxContexts (fun i -> let proxy = pool.RequestLoadContext (mkDeps i) in proxy.Execute(fun m -> m.Id))
        |> Seq.distinct
        |> Seq.length
        |> shouldEqual maxContexts

    [<Fact>]
    let ``07 Should fail when passing max context threshold.`` () =
        use pool = AssemblyLoadContextPoolTester.Init()
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] ; Extension = ".dll" }]
        shouldFailwith<_,OutOfResourcesException>(fun () ->
            for i in 1 .. maxContexts + 1 do
                let proxy = pool.RequestLoadContext (mkDeps i)
                proxy.Execute(fun m -> m.Reset())
                proxy.Execute(fun m -> m.AddTask()))

    [<Fact>]
    let ``08A Should successfully dispose idle contexts when passing time threshold.`` () =
        use pool = AssemblyLoadContextPoolTester.Init()
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] ; Extension = ".dll" }]
        for i in 1 .. 3 * maxContexts do
            let proxy = pool.RequestLoadContext (mkDeps i)
            proxy.Execute(fun m -> m.Reset())

    [<Fact>]
    let ``08B Context count should contract to minimum count after threshold passed.`` () =
        use manager = AssemblyLoadContextPoolTester.Init(threshold = TimeSpan.FromMilliseconds 1.)
        for i in 1 .. 3 * minContexts do
            let _ = manager.RequestLoadContext [idOfWith<int> (byte i)]
            ()

        do Thread.Sleep 1000
        manager.LoadContextCount |> shouldBe (fun c -> c = minContexts)

    [<Fact>]
    let ``09 Should automatically dispose instances in pool with timespan threshold.`` () =
        use pool = AssemblyLoadContextPoolTester.Init(threshold = TimeSpan.FromSeconds 1.)
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] ; Extension = ".dll" }]
        for i in 1 .. maxContexts do
            let mgr = pool.RequestLoadContext (mkDeps i) in ()

        do Thread.Sleep 2000

        pool.LoadContextCount |> shouldEqual pool.MinLoadContexts

    [<Fact>]
    let ``10 Should not automatically dispose busy instances in pool with timespan threshold.`` () =
        use pool = AssemblyLoadContextPoolTester.Init(threshold = TimeSpan.FromSeconds 1.)
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] ; Extension = ".dll" }]
        for i in 1 .. maxContexts do
            let proxy = pool.RequestLoadContext (mkDeps i) in ()
            proxy.Execute(fun m -> m.AddTask())

        do Thread.Sleep 2000

        pool.LoadContextCount |> shouldEqual pool.MaxLoadContexts

    [<Fact>]
    let ``11 Should not return the same LoadContext when task threshold has been reached.`` () =
        use pool = AssemblyLoadContextPoolTester.Init(maxTasks = 1)
        Seq.init maxContexts (fun i -> 
            let proxy = pool.RequestLoadContext [ idOf<int> ]
            proxy.Execute(fun m -> m.AddTask() ; m.Id))
        |> Seq.distinct
        |> Seq.length
        |> shouldEqual maxContexts


    [<Fact>]
    let ``12 AssemblyLoadContextEvaluatorPool simple lambda`` () =
        use pool = AssemblyLoadContextEvaluatorPool.Create(fun () -> printfn "Initializing AssemblyLoadContext")
        pool.Evaluate([], fun () -> 1 + 1) |> shouldEqual 2

    [<Fact>]
    let ``13 AssemblyLoadContextEvaluatorPool simple worfklow`` () =
        use pool = AssemblyLoadContextEvaluatorPool.Create(fun () -> printfn "Initializing AssemblyLoadContext")
        pool.EvaluateAsync([], async { return getLoadContextId() }) 
        |> Async.RunSynchronously 
        |> shouldNotEqual (getLoadContextId())

    [<Fact>]
    let ``14 AssemblyLoadContextEvaluatorPool simple worfklow with exception`` () =
        use pool = AssemblyLoadContextEvaluatorPool.Create(fun () -> printfn "Initializing AssemblyLoadContext")
        Assert.Throws<System.InvalidOperationException>(fun () -> pool.EvaluateAsync([], async { return invalidOp "boom"}) |> Async.RunSynchronously |> ignore) 
        |> ignore

    [<Fact>]
    let ``14B AssemblyLoadContextEvaluatorPool simple worfklow with cancellation`` () =
        use pool = AssemblyLoadContextEvaluatorPool.Create(fun () -> printfn "Initializing AssemblyLoadContext")
        use cts = new CancellationTokenSource()
        Assert.Throws<System.Threading.Tasks.TaskCanceledException>(fun () -> 
            let work = pool.EvaluateAsync([], Async.Sleep 60_000)
            cts.CancelAfter 1000
            Async.RunSynchronously(work, cancellationToken = cts.Token))
        |> ignore


    type LoadContextVagabondLambdaLoaderConfiguration = { CachePath : string }
    
    let config = { CachePath = VagabondConfig.Init().CachePath }

    type LoadContextVagabondLambdaLoader () =
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
        
        interface ILoadContextManager with
            member __.Initialize (config : obj) =
                match config with
                | :? LoadContextVagabondLambdaLoaderConfiguration as c -> VagabondConfig.Init(c.CachePath) |> ignore
                | _ -> ()

            member __.Dispose () = ()
            member __.LastUsed = lastUsed
            member __.TaskCount = taskCount


        static member Eval (vpm : AssemblyLoadContextPool<LoadContextVagabondLambdaLoader>) (f : unit -> 'T) =
            let vg = VagabondConfig.Instance
            let deps = vg.ComputeObjectDependencies(f, true)
            let proxy = vpm.RequestLoadContext(deps |> Seq.map (fun deps -> deps.Id))
            let p = VagabondConfig.Pickler.PickleTyped f
            let presult = proxy.Execute(fun m -> m.Evaluate(deps, p))
            match vg.Serializer.UnPickleTyped presult with
            | Choice1Of2 v -> v
            | Choice2Of2 e -> raise e

        static member Init() = AssemblyLoadContextPool.Create<LoadContextVagabondLambdaLoader> (config)

    [<Fact>]
    let ``15 LoadContext Vagabond Lambda Evaluator`` () =
        use pool = LoadContextVagabondLambdaLoader.Init()
        LoadContextVagabondLambdaLoader.Eval pool (fun () -> 1 + 1) |> shouldEqual 2


    type StaticValueContainer private () =
        static let id = Guid.NewGuid().ToString()
        static member Value = id

    [<Fact>]
    let ``16 LoadContext Vagabond Lambda Evaluator should always use the same context`` () =
        use pool = LoadContextVagabondLambdaLoader.Init()
        Seq.init 10 (fun _ -> LoadContextVagabondLambdaLoader.Eval pool (fun () -> StaticValueContainer.Value))
        |> Seq.distinct
        |> Seq.length
        |> shouldEqual 1