namespace Nessos.Vagrant.Tests

    open System
    open System.Reflection
    open System.IO

    open NUnit.Framework

    open Microsoft.FSharp.Compiler.Interactive.Shell
    open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

    open Nessos.Vagrant

    [<TestFixture>]
    module FsiTests =

        // leave this to force static dependency on LinqOptimizer
        let private linqOptBinder () = Nessos.LinqOptimizer.FSharp.Query.ofSeq [] |> ignore

        // by default, NUnit copies test assemblies to a temp directory
        // use Directory.GetCurrentDirectory to gain access to the original build directory
        let private buildDirectory = Directory.GetCurrentDirectory()
        let getPathLiteral (path : string) =
            let fullPath =
                if Path.IsPathRooted path then path
                else Path.Combine(buildDirectory, path)

            sprintf "@\"%s\"" fullPath

        type FsiEvaluationSession with
        
            member fsi.AddReferences (paths : string list) =
                let directives = 
                    paths 
                    |> Seq.map (fun p -> sprintf "#r %s" <| getPathLiteral p)
                    |> String.concat "\n"

                fsi.EvalInteraction directives

            member fsi.LoadScript (path : string) =
                let directive = sprintf "#load %s" <| getPathLiteral path
                fsi.EvalInteraction directive

            member fsi.TryEvalExpression(code : string) =
                try fsi.EvalExpression(code)
                with _ -> None

        let shouldEqual (expected : 'T) (result : FsiValue option) =
            match result with
            | None -> raise <| new AssertionException(sprintf "expected %A, got exception." expected)
            | Some value ->
                if not <| typeof<'T>.IsAssignableFrom value.ReflectionType then
                    raise <| new AssertionException(sprintf "expected type %O, got %O." typeof<'T> value.ReflectionType)

                match value.ReflectionValue with
                | :? 'T as result when result = expected -> ()
                | result -> raise <| new AssertionException(sprintf "expected %A, got %A." expected result)
            
        type FsiSession private () =
            static let container = ref None

            static member Start () =
                lock container (fun () ->
                    match !container with
                    | Some _ -> invalidOp "an fsi session is already running."
                    | None ->
                        let dummy = new StringReader("")
                        let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
                        let fsi = new FsiEvaluationSession(fsiConfig, [| "fsi.exe" ; "--noninteractive" |], dummy, Console.Out, Console.Error)
                        container := Some fsi; fsi)

            static member Stop () =
                lock container (fun () ->
                    match !container with
                    | None -> invalidOp "No fsi sessions are running"
                    | Some fsi ->
                        // need a 'stop' operation here
                        container := None)


            static member Value =
                match !container with
                | None -> invalidOp "No fsi session is running."
                | Some fsi -> fsi

        let eval (expr : string) = FsiSession.Value.TryEvalExpression expr


        [<TestFixtureSetUp>]
        let initFsiSession () =
            
            let fsi = FsiSession.Start()
            let thisExe = getPathLiteral <| Assembly.GetExecutingAssembly().GetName().Name + ".exe"

            // add dependencies

            fsi.AddReferences 
                [
                    "FsPickler.dll"
                    "Mono.Cecil.dll"
                    "Mono.Reflection.dll"
                    "Vagrant.dll"
                    "LinqOptimizer.Base.dll"
                    "LinqOptimizer.Core.dll"
                    "LinqOptimizer.FSharp.dll"
                    "Vagrant.Tests.exe"
                ]

            fsi.EvalInteraction "open Nessos.Vagrant.Tests.ThunkServer"
            fsi.EvalInteraction <| "let client = ThunkClient.InitLocal(serverExecutable = " + thisExe + ")"

        [<TestFixtureTearDown>]
        let stopFsiSession () =
            FsiSession.Value.Interrupt()
            FsiSession.Value.EvalInteraction "client.Kill()"
            FsiSession.Stop()

        [<Test>]
        let ``01. Simple thunk execution`` () =

            let fsi = FsiSession.Value

            "client.EvaluateThunk <| fun () -> 42" |> fsi.TryEvalExpression |> shouldEqual 42


        [<Test>]
        let ``02. Side effect execution`` () =
            
            let fsi = FsiSession.Value

            fsi.EvalInteraction "open System.IO"
            fsi.EvalInteraction "let randomFile = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())"
            fsi.EvalExpression """client.EvaluateThunk <| fun () -> File.WriteAllText(randomFile, "foo") """ |> shouldEqual ()
            fsi.EvalExpression "File.ReadAllText randomFile" |> shouldEqual "foo"


        [<Test>]
        let ``03. Fsi top-level bindings`` () =
            
            let fsi = FsiSession.Value

            fsi.EvalInteraction("let x = client.EvaluateThunk <| fun () -> [| 1 .. 100 |]")
            fsi.EvalExpression("client.EvaluateThunk <| fun () -> Array.sum x") |> shouldEqual 5050


        [<Test>]
        let ``04. Custom type execution`` () =
            
            let fsi = FsiSession.Value

            fsi.EvalInteraction "type Foo<'T> = { Value : 'T }"
            fsi.EvalInteraction "let x = client.EvaluateThunk <| fun () -> { Value = 41 + 1 }"
            fsi.EvalExpression "x.Value" |> shouldEqual 42


        [<Test>]
        let ``05. Custom functions on custom types`` () =
            
            let fsi = FsiSession.Value

            fsi.EvalInteraction "type Bar<'T> = Bar of 'T"
            fsi.EvalInteraction "module Bar = let map f (Bar x) = Bar (f x)"
            fsi.EvalInteraction "let rec factorial n = if n <= 0 then 1 else n * factorial(n-1)"
            fsi.EvalInteraction "let x = Bar 10"
            fsi.EvalInteraction "let (Bar y) = client.EvaluateThunk <| fun () -> Bar.map factorial x"
            fsi.EvalExpression "y = factorial 10" |> shouldEqual true

        [<Test>]
        let ``06. Custom generic type execution`` () =
            
            let fsi = FsiSession.Value

            fsi.EvalInteraction "type Bar<'T> = Bar of 'T"
            fsi.EvalInteraction "let x = 1"
            fsi.EvalInteraction "let y = 1"
            for i in 1 .. 10 do
                fsi.EvalInteraction "let x = client.EvaluateThunk <| fun () -> Bar x"
                fsi.EvalInteraction "let y = Bar y"

            fsi.EvalExpression "x = y" |> shouldEqual true

        [<Test>]
        let ``07. Asynchronous workflows`` () =

            let code = """

            type Bar<'T> = Bar of 'T
   
            let runAsync (wf : Async<'T>) =
                client.EvaluateThunk <| fun () -> Async.RunSynchronously wf

            let n = 100

            let testWorkflow = async {

                let worker i = async {
                    do printfn "processing job #%d" i
                    return Bar (i+1)
                }

                let! results = [|1..n|] |> Array.map worker |> Async.Parallel

                return results
}
"""
            let fsi = FsiSession.Value
            fsi.EvalInteraction code
            fsi.EvalInteraction "let results = runAsync testWorkflow"
            fsi.EvalExpression "results.Length = n" |> shouldEqual true


        [<Test>]
        let ``08. Deploy LinqOptimizer dynamic assemblies`` () =
        
            let code = """

            open Nessos.LinqOptimizer.FSharp
        
            let nums = [|1..100000|]

            let query = 
                nums
                |> Query.ofSeq
                |> Query.filter (fun num -> num % 2 = 0)
                |> Query.map (fun num -> num * num)
                |> Query.sum
                |> Query.compile
"""

            let fsi = FsiSession.Value
            fsi.EvalInteraction code
            fsi.EvalInteraction "let result = client.EvaluateThunk query"
            fsi.EvalExpression "result = query ()" |> shouldEqual true

        
        [<Test>]
        let ``09. Remotely deploy an actor definition`` () =

            let code = """
            open Microsoft.FSharp.Control
            open Nessos.Vagrant.Tests.TcpActor

            // takes an input, replies with an aggregate sum
            let rec loop state (inbox : MailboxProcessor<int * AsyncReplyChannel<int>>) =
                async {
                    let! msg, rc = inbox.Receive ()

                    printfn "Received %d. Thanks!" msg

                    rc.Reply state

                    return! loop (state + msg) inbox
                }

            let deployActor () = 
                printfn "deploying actor..."
                let a = TcpActor.Create(loop 0, "localhost:18979")
                printfn "done"
                a.GetClient()
"""

            let fsi = FsiSession.Value

            fsi.EvalInteraction code
            fsi.EvalInteraction "let actorRef = client.EvaluateThunk deployActor"
            fsi.EvalInteraction "let postAndReply x = actorRef.PostAndReply <| fun ch -> x,ch"
            fsi.EvalInteraction "do client.UploadDependencies postAndReply"

            for i in 1 .. 100 do
                fsi.EvalInteraction "let _ = postAndReply 1"

            fsi.EvalExpression "postAndReply 0" |> shouldEqual 100

        [<Test>]
        let ``10. Add reference to external library`` () =
            
            let code = """
            
            module StaticAssemblyTest

                type Test<'T> = TestCtor of 'T

                let value = TestCtor (42, "42")
            """

            let scs = new SimpleSourceCodeServices()

            let workDir = Path.GetTempPath()
            let name = Path.GetRandomFileName()
            let sourcePath = Path.Combine(workDir, Path.ChangeExtension(name, ".fs"))
            let assemblyPath = Path.Combine(workDir, Path.ChangeExtension(name, ".dll"))
            
            do File.WriteAllText(sourcePath, code)
            let errors,code = scs.Compile [| "" ; "--target:library" ; sourcePath ; "-o" ; assemblyPath |]
            if code <> 0 then failwithf "Compiler error: %A" errors

            let fsi = FsiSession.Value

            fsi.AddReferences [assemblyPath]
            fsi.EvalInteraction "open StaticAssemblyTest"
            fsi.EvalExpression "client.EvaluateThunk <| fun () -> let (TestCtor (v,_)) = value in v" |> shouldEqual 42

        [<Test>]
        let ``11. Execute code from F# script file`` () =
            
            let code = """
            module Script
            
                type ScriptType<'T> = ScriptCtor of 'T

                let map f (ScriptCtor x) = ScriptCtor (f x)

                let rec factorial n =
                    if n <= 1 then 1
                    else
                        n * factorial (n-1)

                let value = ScriptCtor 10
            """

            let workDir = Path.GetTempPath()
            let name = Path.GetRandomFileName()
            let scriptPath = Path.Combine(workDir, Path.ChangeExtension(name, ".fsx"))
            do File.WriteAllText(scriptPath, code)

            let fsi = FsiSession.Value

            fsi.LoadScript scriptPath
            fsi.EvalInteraction "open Script"
            fsi.EvalInteraction "let r = client.EvaluateThunk <| fun () -> map factorial value"
            fsi.EvalExpression "r = map factorial value" |> shouldEqual true