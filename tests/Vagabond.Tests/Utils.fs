namespace MBrace.Vagabond.Tests

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Compiler.Interactive.Shell
open Xunit
open Xunit.Abstractions
open Xunit.Sdk

[<assembly: CollectionBehavior(DisableTestParallelization = true)>]
do ()

[<AutoOpen>]
module Utils =

    let (@@) left right = Path.Combine(left, right)

    /// root directory of current repository
    let repoRoot = Path.GetFullPath (__SOURCE_DIRECTORY__ @@ "../..")

    let isX64Process = RuntimeInformation.ProcessArchitecture = Architecture.X64
    let isWindowsProcess = RuntimeInformation.IsOSPlatform OSPlatform.Windows

    let shouldFailwith<'T, 'Exn when 'Exn :> exn> (f : unit -> 'T) =
        Assert.Throws<'Exn>(fun () -> f () |> ignore) |> ignore

    let shouldEqual (expected : 'T) (input : 'T) = Assert.Equal<'T>(expected, input)

    let shouldNotEqual (expected : 'T) (input : 'T) = Assert.NotEqual<'T>(expected, input)

    let shouldBe (pred : 'T -> bool) (input : 'T) = Assert.True(pred input, sprintf "value '%A' does not match predicate." input)

    module Path =
        /// for use by fsi evaluators
        let toEscapedString path = path |> Path.GetFullPath |> sprintf "@\"%s\""

    type FsiEvaluationSession with

        member fsi.AddFolderReference (path : string) =
            fsi.EvalInteraction ("#I " + Path.toEscapedString path)
        
        member fsi.AddReferences (paths : string list) =
            let directives = 
                paths 
                |> Seq.map (fun p -> "#r " + Path.toEscapedString p)
                |> String.concat Environment.NewLine

            fsi.EvalInteraction directives

        member fsi.LoadScript (path : string) =
            let directive = "#load " + Path.toEscapedString path
            fsi.EvalInteraction directive

        member fsi.TryEvalExpression(code : string) =
            try fsi.EvalExpression(code)
            with _ -> None

    // Adapted from https://github.com/xunit/samples.xunit/blob/5334ee9cf4a81f40dcb4cafabfeb098a555efb3d/TestOrderExamples/TestCaseOrdering/AlphabeticalOrderer.cs
    type AlphabeticalOrderer() =
        interface ITestCaseOrderer with
            member __.OrderTestCases<'Test when 'Test :> ITestCase>(tests : seq<'Test>) =
                tests |> Seq.sortBy(fun t -> t.TestMethod.Method.Name)

    type ObservableTextWriter() =
        inherit TextWriter()

        let event = new Event<string>()
        let sb = new System.Text.StringBuilder()

        let handleBuffer (buffer : char[]) (offset : int) (count : int) =
            for i = offset to offset + count - 1 do
                match buffer.[i] with
                | '\r' -> ()
                | '\n' -> event.Trigger(sb.ToString()) ; sb.Clear() |> ignore
                |   c  -> sb.Append c |> ignore

        member __.Lines = event.Publish :> IObservable<string>

        override __.Flush() = event.Trigger(sb.ToString()) ; sb.Clear() |> ignore
        override __.Encoding = System.Text.Encoding.UTF8
        override __.Write(buffer : char[]) = handleBuffer buffer 0 buffer.Length
        override __.WriteAsync(buffer : char[], offset : int, count : int) : Task = 
            handleBuffer buffer offset count ; Task.CompletedTask

        interface IDisposable with member __.Dispose() = __.Flush()