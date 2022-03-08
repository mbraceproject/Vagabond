namespace MBrace.Vagabond.Tests

open System
open System.Collections.Concurrent
open System.IO

open Xunit
open Xunit.Abstractions

open FSharp.Compiler.Interactive.Shell
open FSharp.Compiler.SourceCodeServices

type FsiSessionFixture() =
    static do
        VagabondConfig.Init() |> ignore
        Actor.Init()

    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    let inReader = new StringReader("")
    let stdoutWriter = new ObservableTextWriter()
    let stderrWriter = new ObservableTextWriter()
    let outputLines = Observable.merge stdoutWriter.Lines stderrWriter.Lines
    let fsi = FsiEvaluationSession.Create(fsiConfig, [| "fsi.exe" ; "--noninteractive" |], inReader, stdoutWriter, stderrWriter)

    /// capture and report any fsi output in event of exception
    let protectInteraction f =
        let queue = new ConcurrentQueue<string>()
        try use _d = outputLines.Subscribe queue.Enqueue in f ()
        with e ->
            let output = queue |> String.concat Environment.NewLine
            raise <| Exception(sprintf "Fsi error:\n%s" output, e)

    do protectInteraction (fun () -> FsiSessionFixture.SetupThunkServer(fsi))

    member __.Session = fsi

    // a bit of Voodoo to get xunit to print the damn fsi output logs
    member __.Subscribe(output : ITestOutputHelper) = outputLines.Subscribe output.WriteLine

    interface IDisposable with
        member __.Dispose() =
            protectInteraction (fun () -> FsiSessionFixture.TearDownThunkServer(fsi))
            stdoutWriter.Dispose()
            stderrWriter.Dispose()
    
    static member private SetupThunkServer(fsi : FsiEvaluationSession) =
        
        let thunkServerPath = 
            let configurationPath = 
                let assemblyLocation = System.Environment.CurrentDirectory
                Path.Combine(
                    Path.GetFileName(Path.GetDirectoryName(assemblyLocation)), 
                    Path.GetFileName assemblyLocation)

        
            repoRoot @@ sprintf "samples/ThunkServer/bin/%s" configurationPath

        let thunkServerExe = thunkServerPath @@ "ThunkServer"

        // add dependencies

        fsi.AddFolderReference thunkServerPath

        fsi.AddReferences 
            [
                thunkServerPath @@ "FsPickler.dll"
                thunkServerPath @@ "Vagabond.dll"
                thunkServerPath @@ "Thespian.dll"
#if NETFRAMEWORK
                thunkServerPath @@ "ThunkServer.exe"
#else
                thunkServerPath @@ "ThunkServer.dll"
#endif

                repoRoot @@ "packages/fsi/LinqOptimizer.FSharp/lib/netstandard2.0/LinqOptimizer.Base.dll"
                repoRoot @@ "packages/fsi/LinqOptimizer.FSharp/lib/netstandard2.0/LinqOptimizer.Core.dll"
                repoRoot @@ "packages/fsi/LinqOptimizer.FSharp/lib/netstandard2.0/LinqOptimizer.FSharp.dll"
                repoRoot @@ "packages/fsi/MathNet.Numerics/lib/netstandard2.0/MathNet.Numerics.dll"
                repoRoot @@ "packages/fsi/MathNet.Numerics.FSharp/lib/netstandard2.0/MathNet.Numerics.FSharp.dll"
                repoRoot @@ "resource/Google.OrTools.dll"
            ]

        fsi.EvalInteraction "open ThunkServer"
        fsi.EvalInteraction ("ThunkClient.Executable <- " + Path.toEscapedString thunkServerExe)
        fsi.EvalInteraction "let client = ThunkClient.InitLocal()"

    static member private TearDownThunkServer(fsi : FsiEvaluationSession) =
        fsi.EvalInteraction "client.Kill()"


[<TestCaseOrderer("MBrace.Vagabond.Tests.Utils.AlphabeticalOrderer", "Vagabond.Tests")>]
type FsiTests(fixture : FsiSessionFixture, output : ITestOutputHelper) =
    let testOutputToken = fixture.Subscribe output
    let fsi = fixture.Session

    let defineQuotationEvaluator (fsi : FsiEvaluationSession) =
        fsi.EvalInteraction """
            open Microsoft.FSharp.Quotations
            open Microsoft.FSharp.Linq.RuntimeHelpers

            let eval (e : Expr<'T>) = LeafExpressionConverter.EvaluateQuotation e :?> 'T
        """

    let shouldEqual (expected : 'T) (result : FsiValue option) =
        match result with
        | None -> Assert.True(false, sprintf "expected %A, got exception." expected)
        | Some value ->
            match value.ReflectionValue with
            | :? 'T as result -> Assert.Equal<'T>(expected, result)
            | _ -> Assert.True(false, sprintf "expected %A, got %A." expected result)

    [<Fact>]
    let ``01 Simple thunk execution`` () =
        "client.EvaluateThunk <| fun () -> 42" |> fsi.TryEvalExpression |> shouldEqual 42

    [<Fact>]
    let ``02 Side effect execution`` () =
        fsi.EvalInteraction "open System.IO"
        fsi.EvalInteraction "let randomFile = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())"
        fsi.EvalExpression """client.EvaluateThunk <| fun () -> File.WriteAllText(randomFile, "foo") """ |> shouldEqual ()
        fsi.EvalExpression "File.ReadAllText randomFile" |> shouldEqual "foo"


    [<Fact>]
    let ``03 Fsi top-level bindings`` () =
        fsi.EvalInteraction("let x = client.EvaluateThunk <| fun () -> [| 1 .. 100 |]")
        fsi.EvalExpression("client.EvaluateThunk <| fun () -> Array.sum x") |> shouldEqual 5050

    [<Fact>]
    let ``03 Fsi large top-level bindings`` () =
        for i in 1 .. 5 do
            fsi.EvalInteraction("let x = [| 1L .. 1000000L |]")
            fsi.EvalExpression("client.EvaluateThunk <| fun () -> Array.length x") |> shouldEqual 1000000

    [<Fact>]
    let ``04 Custom type execution`` () =
        fsi.EvalInteraction "type Foo = { Value : int }"
        fsi.EvalInteraction "let x = client.EvaluateThunk <| fun () -> { Value = 41 + 1 }"
        fsi.EvalExpression "x.Value" |> shouldEqual 42

    [<Fact>]
    let ``05 Custom generic type execution`` () =
        fsi.EvalInteraction "type Bar<'T> = Bar of 'T"
        fsi.EvalInteraction "let x = 1"
        fsi.EvalInteraction "let y = 1"
        for i in 1 .. 10 do
            fsi.EvalInteraction "let x = client.EvaluateThunk <| fun () -> Bar x"
            fsi.EvalInteraction "let y = Bar y"

        fsi.EvalExpression "x = y" |> shouldEqual true

    [<Fact>]
    let ``06 Custom functions on custom types`` () =
        fsi.EvalInteraction "type Bar<'T> = Bar of 'T"
        fsi.EvalInteraction "module Bar = let map f (Bar x) = Bar (f x)"
        fsi.EvalInteraction "let rec factorial n = if n <= 0 then 1 else n * factorial(n-1)"
        fsi.EvalInteraction "let x = Bar 10"
        fsi.EvalInteraction "let (Bar y) = client.EvaluateThunk <| fun () -> Bar.map factorial x"
        fsi.EvalExpression "y = factorial 10" |> shouldEqual true

    [<Fact>]
    let ``07 Nested module definitions`` () =

        let code = """
            module NestedModule =
                
                type NestedType = Value of int

                let x = Value 41
        """

        fsi.EvalInteraction code
        fsi.EvalInteraction "open NestedModule"
        fsi.EvalInteraction "let (Value y) = client.EvaluateThunk <| fun () -> let (Value y) = x in Value (y+1)" 
        fsi.EvalExpression "y" |> shouldEqual 42


    [<Fact>]
    let ``09 Class Definitions`` () =
        let code = """
            type Cell<'T> (x : 'T) =
                let mutable x = x
                member __.Value
                    with get () = x
                    and set y = x <- y
        """

        fsi.EvalInteraction code
        fsi.EvalInteraction "let c = Cell<int>(41)"
        fsi.EvalInteraction "let c' = client.EvaluateThunk <| fun () -> c.Value <- c.Value + 1 ; c"
        fsi.EvalExpression "c'.Value" |> shouldEqual 42


    [<Fact>]
    let ``10 Asynchronous workflows`` () =

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

        fsi.EvalInteraction code
        fsi.EvalInteraction "let results = runAsync testWorkflow"
        fsi.EvalExpression "results.Length = n" |> shouldEqual true


    [<Fact(Skip = "Failing with latest LinqOptimizer")>]
    let ``11 Deploy LinqOptimizer dynamic assemblies`` () =
        
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

        fsi.EvalInteraction code
        fsi.EvalInteraction "let result = client.EvaluateThunk query"
        fsi.EvalExpression "result = query ()" |> shouldEqual true

    [<Fact>]
    let ``12 Remotely deploy an actor definition`` () =
        let code = """
            open Nessos.Thespian

            type Counter =
                | Increment of int
                | GetCount of IReplyChannel<int>

            let rec loop state (self : Actor<Counter>) =
                async {
                    let! msg = self.Receive ()
                    match msg with
                    | Increment i -> 
                        printfn "Increment by %d" i
                        return! loop (i + state) self
                    | GetCount rc ->
                        do! rc.Reply state
                        return! loop state self
                }

            let deployActor (behaviour : Actor<'T> -> Async<unit>) : ActorRef<'T> = 
                client.EvaluateThunk(fun () ->
                    printfn "deploying actor..."
                    let actor = Actor.bind behaviour |> Actor.Publish
                    actor.Ref)
        """

        fsi.EvalInteraction code
        fsi.EvalInteraction "let actorRef = deployActor (loop 0)"

        for i in 1 .. 100 do
            fsi.EvalInteraction "actorRef <-- Increment 1"

        fsi.EvalExpression "actorRef <!= GetCount" |> shouldEqual 100

    [<Fact>]
    let ``13 Add reference to external library`` () =
        if not isWindowsProcess then () else // fsc.exe is difficult to set up on linux
        let code = """
            module StaticAssemblyTest

                type Test<'T> = TestCtor of 'T

                let value = TestCtor (42, "42")
        """

        let fsc = FSharpChecker.Create()

        let workDir = Path.GetTempPath()
        let name = Path.GetRandomFileName()
        let sourcePath = Path.Combine(workDir, Path.ChangeExtension(name, ".fs"))
        let assemblyPath = Path.Combine(workDir, Path.ChangeExtension(name, ".dll"))
            
        do File.WriteAllText(sourcePath, code)
        let errors,code = fsc.Compile [| "fsc.exe" ; sourcePath ; "-o" ; assemblyPath ; "--target:library" |] |> Async.RunSynchronously
        if code <> 0 then failwithf "Compiler error: %A" errors

        fsi.AddReferences [assemblyPath]
        fsi.EvalInteraction "open StaticAssemblyTest"
        fsi.EvalExpression "client.EvaluateThunk <| fun () -> let (TestCtor (v,_)) = value in v" |> shouldEqual 42

    [<Fact>]
    let ``14 Execute code from F# script file`` () =
            
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

        fsi.LoadScript scriptPath
        fsi.EvalInteraction "open Script"
        fsi.EvalInteraction "let r = client.EvaluateThunk <| fun () -> map factorial value"
        fsi.EvalExpression "r = map factorial value" |> shouldEqual true

    [<Fact>]
    let ``15 Single Interaction - Multiple executions`` () =
            
        let code = """
            let cell = ref 0
            for i in 1 .. 100 do
                cell := client.EvaluateThunk <| fun () -> cell.Value + 1

            cell.Value
        """

        fsi.EvalExpression code |> shouldEqual 100

    [<Fact>]
    let ``16 Cross slice inheritance`` () =
            
        let type1 = """
            type Foo<'T>(x : 'T) =
                member __.Value = x
        """

        let type2 = """
            type Bar<'T, 'S>(x : 'T, y : 'S) =
                inherit Foo<'T>(x)
                
                member __.Value2 = y
        """

        let type3 = """
            type Baz(x : int, y : string) =
                inherit Bar<int, string>(x,y)

                member __.Pair = x,y
        """

        fsi.EvalInteraction type1
        fsi.EvalInteraction "client.EvaluateThunk <| fun () -> new Foo<int>(42)"
        fsi.EvalInteraction type2
        fsi.EvalInteraction "client.EvaluateThunk <| fun () -> new Bar<int, bool>(42, false)"
        fsi.EvalInteraction type3
        fsi.EvalInteraction """let foo = client.EvaluateThunk <| fun () -> let b = new Baz(42, "42") in b :> Foo<int>"""
        fsi.EvalExpression "foo.Value" |> shouldEqual 42

    [<Fact>]
    let ``17 Cross slice interfaces`` () =

        let interfaceCode = """
            type IFoo =
                abstract GetEncoding<'T> : 'T -> int

            let eval (foo : IFoo) (x : 'T) = foo.GetEncoding x
        """
        let implementationCode = """
            type Foo () =
                interface IFoo with
                    member __.GetEncoding<'T> (x : 'T) = 42
        """

        fsi.EvalInteraction interfaceCode
        fsi.EvalInteraction "client.EvaluateThunk <| fun () -> typeof<IFoo>.GetHashCode()"
        fsi.EvalInteraction implementationCode
        fsi.EvalExpression "client.EvaluateThunk <| fun () -> eval (new Foo()) [1..100]" |> shouldEqual 42

    [<Fact>]
    let ``17B Interfaces inheriting interfaces`` () =

        let code = """
            type IFoo =
                abstract Foo : int

            type IBar =
                inherit IFoo
                abstract Bar : int

            let mkBar () = 
                { new IBar with
                    member __.Foo = 15
                    member __.Bar = 27 }
        """

        fsi.EvalInteraction code
        fsi.EvalExpression "client.EvaluateThunk <| fun () -> mkBar().Foo + mkBar().Bar" |> shouldEqual 42

    [<Fact>]
    let ``18 Binary Trees`` () =
        let code = """
            type BinTree<'T> = Leaf | Node of 'T * BinTree<'T> * BinTree<'T>

            let rec init = function 0 -> Leaf | n -> Node(n, init (n-1), init (n-1))
            let rec map f = function Leaf -> Leaf | Node(a,l,r) -> Node(f a, map f l, map f r)
            let rec reduce id f = function Leaf -> id | Node(a,l,r) -> f (f (reduce id f l) a) (reduce id f r)

            let tree = client.EvaluateThunk <| fun () -> init 5

            let tree' = client.EvaluateThunk <| fun () -> map (fun x -> 2. ** float x) tree

            let sum = client.EvaluateThunk <| fun () -> reduce 1. (+) tree'
        """

        fsi.EvalInteraction code
        fsi.EvalExpression "sum" |> shouldEqual 192.


    [<Fact>]
    let ``19 Native dependencies`` () =
        if isWindowsProcess && isX64Process then

            let code = """
                open MathNet.Numerics
                open MathNet.Numerics.LinearAlgebra

                let getRandomDeterminant () =
                    let m = Matrix<double>.Build.Random(200,200) 
                    m.LU().Determinant

                client.EvaluateThunk getRandomDeterminant
            """

            fsi.EvalInteraction code

            // register native dll's

            let nativeDir = repoRoot @@ "packages/fsi/MathNet.Numerics.MKL.Win-x64/build/x64/"
            let libiomp5md = nativeDir + "libiomp5md.dll"
            let mkl = nativeDir + "MathNet.Numerics.MKL.dll"

            fsi.EvalInteraction <| "client.RegisterNativeDependency " + Path.toEscapedString libiomp5md
            fsi.EvalInteraction <| "client.RegisterNativeDependency " + Path.toEscapedString mkl

            let code' = """
                let useNativeMKL () = Control.UseNativeMKL()
                client.EvaluateThunk (fun () -> useNativeMKL () ; getRandomDeterminant ())
            """

            fsi.EvalInteraction code'

    [<Fact>]
    let ``20 Update earlier bindings if value changed`` () =
        fsi.EvalInteraction "let array = [|1..100|]"

        fsi.EvalExpression "client.EvaluateThunk (fun () -> Array.sum array)" |> shouldEqual 5050

        fsi.EvalInteraction "array.[49] <- 0"
        fsi.EvalExpression "client.EvaluateThunk (fun () -> Array.sum array)" |> shouldEqual 5000

        fsi.EvalInteraction "array.[49] <- 50"
        fsi.EvalExpression "client.EvaluateThunk (fun () -> Array.sum array)" |> shouldEqual 5050

    [<Fact>]
    let ``21 Concurrent calls to client`` () =
        let fsi = fixture.Session
        fsi.EvalInteraction "[|1..20|] |> Array.Parallel.map (fun i -> client.EvaluateThunk (fun () -> i * i))"

    [<Fact>]
    let ``22 Simple quotation literal`` () =
        fsi.EvalInteraction "client.EvaluateThunk (fun () -> <@ 1 + 1 @>)"

    [<Fact>]
    let ``23 Quotation literal references fsi code`` () =
        fsi.EvalInteraction "let rec f n = if n <= 1 then n else f(n-2) + f(n-1)"
        fsi.EvalInteraction "client.EvaluateThunk(fun () -> <@ f 10 @>)"

    [<Fact>]
    let ``24 Parametric quotation evaluation`` () =
        defineQuotationEvaluator fsi
        fsi.EvalInteraction "let incr x = eval <@ x + 1 @>"
        fsi.EvalExpression "client.EvaluateThunk(fun () -> incr 41)" |> shouldEqual 42

    [<Fact>]
    let ``24 Spliced quotation evaluation`` () =
        defineQuotationEvaluator fsi
        fsi.EvalInteraction """
            let rec expand n =
                if n = 0 then <@ 0 @>
                else <@ %(expand (n-1)) + 1 @>
        """

        fsi.EvalExpression "client.EvaluateThunk(fun () -> let e = expand 10 in eval <@ %e + 32 @>)" |> shouldEqual 42

    [<Fact>]
    let ``25 Cross slice quotation reference`` () =
        fsi.EvalInteraction "let x = client.EvaluateThunk(fun () -> 1 + 1)"
        fsi.EvalExpression "client.EvaluateThunk(fun () -> eval <@ x @>)" |> shouldEqual 2

    [<Fact>]
    let ``26 Class static field pickling`` () =
        fsi.EvalInteraction """
            type Foo () =
                static let mutable count = 0
                do count <- count + 1
                static member Count = count
        """

        fsi.EvalInteraction "let x = new Foo ()"
        fsi.EvalExpression "client.EvaluateThunk (fun () -> Foo.Count)" |> shouldEqual 1
        fsi.EvalInteraction "let y = new Foo ()"
        fsi.EvalExpression "client.EvaluateThunk (fun () -> Foo.Count)" |> shouldEqual 2


    [<Fact>]
    let ``26 Multiple Large bindings in single slice`` () =
        fsi.EvalInteraction """
            let x = [|1L .. 10000000L|]
            let y = [|1L .. 10000000L|]
        """

        fsi.EvalExpression "client.EvaluateThunk (fun () -> x.Length = y.Length)" |> shouldEqual true

    [<Fact>]
    let ``27 Mixed mode assemblies`` () =
        if isWindowsProcess && isX64Process then
            fsi.EvalInteraction """
            open Google.OrTools
            open Google.OrTools.Algorithms

            let vector (ints : seq<int64>) =
                let k = new KInt64Vector()
                for i in ints do k.Add i
                k

            let vectorvector (ints : seq<seq<int64>>) =
                let kvv = new KInt64VectorVector()
                for g in ints do kvv.Add(vector g)
                kvv

            let solve() =
                let kss = new Google.OrTools.Algorithms.KnapsackSolver("default")
                kss.Init(vector [1L .. 10L], vectorvector [[1L]], vector [1L .. 10L])
                kss.Solve()
"""

            fsi.EvalInteraction "let expected = solve ()"
            fsi.EvalExpression "client.EvaluateThunk (fun () -> solve () = expected)" |> shouldEqual true

    [<Fact>]
    let ``28 Should be able to serialize new types without explicitly requesting slice compilation`` () =
        fsi.EvalInteraction """
            type LatestTypeDefinition = { Value : int }

            let roundTrip (n : int) =
                let v = { Value = n }
                let p = VagabondConfig.Serializer.Pickle(box v)
                let v' = client.EvaluateThunk(fun () -> VagabondConfig.Serializer.UnPickle<obj>(p) :?> LatestTypeDefinition)
                v'.Value
"""

        fsi.EvalExpression "roundTrip 42" |> shouldEqual 42


    [<Fact>]
    let ``29 Should correctly evaluate Array2D-init calls`` () =
        fsi.EvalInteraction """
            let array = client.EvaluateThunk(fun () -> Array2D.init 5 5 (fun _ _ -> 0.))
        """

        fsi.EvalExpression "array.GetLength(0)" |> shouldEqual 5
        fsi.EvalExpression "array.GetLength(1)" |> shouldEqual 5

    [<Fact>]
    let ``30 Test against Cecil bug #278`` () =
        // see https://github.com/jbevain/cecil/issues/278
        fsi.EvalInteraction """
        type A<'T> = class end
        type B<'T,'U> = class end

        type C =
            static member G x : A<'T> = failwith ""
            static member F x : A<'T> = C.G (fun () -> x)

        type D =
            static member F (x:A<B<_,_>>) = C.F x
    """

        fsi.EvalExpression "client.EvaluateThunk(fun () -> typeof<D>)" |> shouldBe (fun x -> true)


    [<Fact>]
    let ``31 F# 4-1 Struct types supported`` () =
        fsi.EvalInteraction """
        
        [<Struct>]
        type A = A1 of a:int | A2 of b:int * string
        
        [<Struct>]
        type B = { A : A ; B : struct(int * int) }
"""

        fsi.EvalExpression """client.EvaluateThunk(fun () -> { A = A2(42,"42") ; B = struct(1,2) })""" |> shouldBe (fun x -> true)

    [<Fact>]
    let ``32 F# 4-5 Anon records supported`` () =
        fsi.EvalExpression """client.EvaluateThunk(fun () -> {| A = {| x = 2 |} ; B = struct(1,2) |})""" |> shouldBe (fun x -> true)

    interface IDisposable with member __.Dispose() = testOutputToken.Dispose()
    interface IClassFixture<FsiSessionFixture>
