namespace Nessos.Vagabond.Tests

open System
open System.Threading

open NUnit.Framework

open Nessos.Vagabond
open Nessos.Vagabond.AppDomainIsolation

[<TestFixture>]
module ``AppDomain Isolation Tests`` =

    let minDomains = 3
    let maxDomains = 20

    type AppDomainIsolationTester () =
        inherit MarshalByRefObject()

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

        interface IAppDomainClient with
            member __.Init () = isInitialized <- true
            member __.Fini () = isFinalized <- true
            member __.TaskCount = taskCount
            member __.LastUsed = lastUsed

        static member CreateManager(?threshold) = 
            AppDomainIsolationManager.Create<AppDomainIsolationTester>(minDomains, maxDomains, ?threshold = threshold)

    let (!) (client : AppDomainIsolationTester) = client :> IAppDomainClient

    let idOf<'T> = Utilities.ComputeAssemblyId typeof<'T>.Assembly

    [<Test>]
    let ``01. Domain count should be more than minimum count.`` () =
        use manager = AppDomainIsolationTester.CreateManager()
        manager.DomainCount |> shouldBe (fun c -> c >= minDomains)

    [<Test>]
    let ``02. Simple init and tear down an application domain.`` () =
        use manager = AppDomainIsolationTester.CreateManager()
        let dependencies = [ idOf<int> ; idOf<int option> ]
        let client = manager.GetAppDomainClient dependencies
        client.IsInitialized |> shouldEqual true
        client.IsFinalized |> shouldEqual false
        (! client).TaskCount |> shouldEqual 0
        client.AddTask ()
        (! client).TaskCount |> shouldEqual 1
        client.RemoveTask ()
        (! client).TaskCount |> shouldEqual 0

    [<Test>]
    let ``03. Should use the same AppDomain when using identical dependency sets.`` () =
        use manager = AppDomainIsolationTester.CreateManager()
        let dependencies = [ idOf<int> ; idOf<int option> ]
        let client = manager.GetAppDomainClient dependencies
        for i = 1 to 10 do
            let client' = manager.GetAppDomainClient dependencies
            client'.Id |> shouldEqual client.Id

    [<Test>]
    let ``04. Should use the same AppDomain when extending the dependency set.`` () =
        use manager = AppDomainIsolationTester.CreateManager()
        let dependencies = [ idOf<int> ]
        let dependencies' = idOf<int option> :: dependencies
        let client = manager.GetAppDomainClient dependencies
        for i = 1 to 10 do
            let client' = manager.GetAppDomainClient dependencies'
            client'.Id |> shouldEqual client.Id

    [<Test>]
    let ``05. Should use separate AppDomain when using incompatible dependency sets.`` () =
        use manager = AppDomainIsolationTester.CreateManager()
        let dependencies = [ idOf<int> ; idOf<int option> ]
        let client = manager.GetAppDomainClient dependencies
        let dependencies' = [ idOf<int> ; { idOf<int option> with ImageHash = [||] } ]
        for i = 1 to 10 do
            let client' = manager.GetAppDomainClient dependencies'
            client'.Id |> shouldNotEqual client.Id

    [<Test>]
    let ``06. Should create new AppDomains when conflicting dependencies.`` () =
        use manager = AppDomainIsolationTester.CreateManager()
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] }]
        Seq.init maxDomains (fun i -> let client = manager.GetAppDomainClient (mkDeps i) in client.Id)
        |> Seq.distinct
        |> Seq.length
        |> shouldEqual maxDomains

    [<Test>]
    let ``07. Should fail when passing max domain threshold.`` () =
        use manager = AppDomainIsolationTester.CreateManager()
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] }]
        shouldFailwith<_,OutOfResourcesException>(fun () ->
            for i in 1 .. maxDomains + 1 do
                let client = manager.GetAppDomainClient (mkDeps i)
                client.Reset() ; client.AddTask())

    [<Test>]
    let ``08. Should successfully dispose idle domains when passing domain threshold.`` () =
        use manager = AppDomainIsolationTester.CreateManager()
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] }]
        for i in 1 .. 3 * maxDomains do
            let client = manager.GetAppDomainClient (mkDeps i)
            client.Reset()

    [<Test>]
    let ``09. Should automatically dispose instances in manager with timespan threshold.`` () =
        use manager = AppDomainIsolationTester.CreateManager(threshold = TimeSpan.FromSeconds 1.)
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] }]
        for i in 1 .. maxDomains do
            let client = manager.GetAppDomainClient (mkDeps i) in ()

        do Thread.Sleep 2000

        manager.DomainCount |> shouldEqual manager.MinDomains

    [<Test>]
    let ``10. Should not automatically dispose busy instances in manager with timespan threshold.`` () =
        use manager = AppDomainIsolationTester.CreateManager(threshold = TimeSpan.FromSeconds 1.)
        let name = Guid.NewGuid().ToString()
        let mkDeps i = [{ FullName = name ; ImageHash = [|byte i|] }]
        for i in 1 .. maxDomains do
            let client = manager.GetAppDomainClient (mkDeps i) in ()
            client.AddTask()

        do Thread.Sleep 2000

        manager.DomainCount |> shouldEqual manager.MaxDomains