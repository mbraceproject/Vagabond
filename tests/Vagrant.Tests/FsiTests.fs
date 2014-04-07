namespace Nessos.Vagrant.Tests

    open System
    open System.Reflection
    open System.IO

    open NUnit.Framework

    open Microsoft.FSharp.Compiler.Interactive.Shell
    open Microsoft.FSharp.Compiler.ErrorLogger

    open Nessos.Vagrant

    [<TestFixture>]
    module FsiTests =

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
        
            member fsi.AddReference (path : string) =
                let directive = sprintf "#r %s" <| getPathLiteral path
                fsi.EvalInteraction directive

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

            static member Value =
                match !container with
                | None -> invalidOp "No fsi session is running."
                | Some fsi -> fsi

        let eval (expr : string) = FsiSession.Value.TryEvalExpression expr


        [<TestFixtureSetUp>]
        let initFsiSession () =
            
            let fsi = FsiSession.Start()
            let thisExe = getPathLiteral <| Assembly.GetExecutingAssembly().GetName().Name + ".exe"

            // need access

            // add dependencies

            fsi.AddReference "FsPickler.dll"
            fsi.AddReference "Mono.Cecil.dll"
            fsi.AddReference "Mono.Reflection.dll"
            fsi.AddReference "Vagrant.dll"
            fsi.AddReference "Vagrant.Tests.exe"

            fsi.EvalInteraction "open Nessos.Vagrant.Tests.ThunkServer"
            fsi.EvalInteraction <| "let client = ThunkClient.InitLocal(serverExecutable = " + thisExe + ")"

        [<TestFixtureTearDown>]
        let stopFsiSession () = ()

        [<Test>]
        let ``1. Simple thunk execution`` () =

            let fsi = FsiSession.Value

            "client.EvaluateThunk <| fun () -> 42" |> fsi.TryEvalExpression |> shouldEqual 42


        [<Test>]
        let ``2. Side effect execution`` () =
            
            let fsi = FsiSession.Value

            fsi.EvalInteraction "open System.IO"
            fsi.EvalInteraction "let randomFile = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())"
            fsi.EvalExpression """client.EvaluateThunk <| fun () -> File.WriteAllText(randomFile, "foo") """ |> shouldEqual ()
            fsi.EvalExpression "File.ReadAllText randomFile" |> shouldEqual "foo"