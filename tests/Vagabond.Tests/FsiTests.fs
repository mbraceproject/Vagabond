namespace MBrace.Vagabond.Tests

open System
open System.Reflection
open System.IO

open NUnit.Framework

open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices

open MBrace.Vagabond

[<TestFixture>]
module FsiTests =

    let is64BitProcess = IntPtr.Size = 8

    let runsOnMono = lazy(Type.GetType("Mono.Runtime") <> null)

    let getFullPath path = 
        let fullPath = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", path)
        sprintf "@\"%s\"" fullPath

    type FsiEvaluationSession with
        
        member fsi.AddReferences (paths : string list) =
            let directives = 
                paths 
                |> Seq.map (fun p -> "#r " + getFullPath p)
                |> String.concat Environment.NewLine

            fsi.EvalInteraction directives

        member fsi.LoadScript (path : string) =
            let directive = "#load " + getFullPath path
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
                    let fsi = FsiEvaluationSession.Create(fsiConfig, [| "fsi.exe" ; "--noninteractive" |], dummy, Console.Out, Console.Error)
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

    let defineQuotationEvaluator (fsi : FsiEvaluationSession) =
        fsi.EvalInteraction """
            open Microsoft.FSharp.Quotations
            open Microsoft.FSharp.Linq.RuntimeHelpers

            let eval (e : Expr<'T>) = LeafExpressionConverter.EvaluateQuotation e :?> 'T
        """


    [<OneTimeSetUp>]
    let initFsiSession () =
            
        VagabondConfig.Init()
        Actor.Init()

        let fsi = FsiSession.Start()

        let thunkServer = getFullPath "bin/ThunkServer.exe"

        // add dependencies

        fsi.AddReferences 
            [
                "bin/FsPickler.dll"
                "bin/Mono.Cecil.dll"
                "bin/Vagabond.dll"
                "bin/Thespian.dll"
                "bin/ThunkServer.exe"

                "packages/testing/LinqOptimizer.FSharp/lib/LinqOptimizer.Base.dll"
                "packages/testing/LinqOptimizer.FSharp/lib/LinqOptimizer.Core.dll"
                "packages/testing/LinqOptimizer.FSharp/lib/LinqOptimizer.FSharp.dll"
                "packages/testing/MathNet.Numerics/lib/net40/MathNet.Numerics.dll"
                "packages/testing/MathNet.Numerics.FSharp/lib/net40/MathNet.Numerics.FSharp.dll"
                "resource/Google.OrTools.dll"
            ]

        fsi.EvalInteraction "open ThunkServer"
        fsi.EvalInteraction <| "ThunkClient.Executable <- " + thunkServer
        fsi.EvalInteraction "let client = ThunkClient.InitLocal()"

    [<OneTimeTearDown>]
    let stopFsiSession () =
        FsiSession.Value.Interrupt()
        FsiSession.Value.EvalInteraction "client.Kill()"
        FsiSession.Stop()

    [<Test>]
    let ``01 Simple thunk execution`` () =

        let fsi = FsiSession.Value

        "client.EvaluateThunk <| fun () -> 42" |> fsi.TryEvalExpression |> shouldEqual 42


    [<Test>]
    let ``02 Side effect execution`` () =
            
        let fsi = FsiSession.Value

        fsi.EvalInteraction "open System.IO"
        fsi.EvalInteraction "let randomFile = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())"
        fsi.EvalExpression """client.EvaluateThunk <| fun () -> File.WriteAllText(randomFile, "foo") """ |> shouldEqual ()
        fsi.EvalExpression "File.ReadAllText randomFile" |> shouldEqual "foo"


    [<Test>]
    let ``03 Fsi top-level bindings`` () =
            
        let fsi = FsiSession.Value

        fsi.EvalInteraction("let x = client.EvaluateThunk <| fun () -> [| 1 .. 100 |]")
        fsi.EvalExpression("client.EvaluateThunk <| fun () -> Array.sum x") |> shouldEqual 5050

    [<Test>]
    let ``03 Fsi large top-level bindings`` () =
            
        let fsi = FsiSession.Value

        for i in 1 .. 5 do
            fsi.EvalInteraction("let x = [| 1L .. 1000000L |]")
            fsi.EvalExpression("client.EvaluateThunk <| fun () -> Array.length x") |> shouldEqual 1000000

    [<Test>]
    let ``04 Custom type execution`` () =
            
        let fsi = FsiSession.Value

        fsi.EvalInteraction "type Foo = { Value : int }"
        fsi.EvalInteraction "let x = client.EvaluateThunk <| fun () -> { Value = 41 + 1 }"
        fsi.EvalExpression "x.Value" |> shouldEqual 42

    [<Test>]
    let ``05 Custom generic type execution`` () =
            
        let fsi = FsiSession.Value

        fsi.EvalInteraction "type Bar<'T> = Bar of 'T"
        fsi.EvalInteraction "let x = 1"
        fsi.EvalInteraction "let y = 1"
        for i in 1 .. 10 do
            fsi.EvalInteraction "let x = client.EvaluateThunk <| fun () -> Bar x"
            fsi.EvalInteraction "let y = Bar y"

        fsi.EvalExpression "x = y" |> shouldEqual true

    [<Test>]
    let ``06 Custom functions on custom types`` () =
            
        let fsi = FsiSession.Value

        fsi.EvalInteraction "type Bar<'T> = Bar of 'T"
        fsi.EvalInteraction "module Bar = let map f (Bar x) = Bar (f x)"
        fsi.EvalInteraction "let rec factorial n = if n <= 0 then 1 else n * factorial(n-1)"
        fsi.EvalInteraction "let x = Bar 10"
        fsi.EvalInteraction "let (Bar y) = client.EvaluateThunk <| fun () -> Bar.map factorial x"
        fsi.EvalExpression "y = factorial 10" |> shouldEqual true

    [<Test>]
    let ``07 Nested module definitions`` () =

        let code = """
            module NestedModule =
                
                type NestedType = Value of int

                let x = Value 41
        """
            
        let fsi = FsiSession.Value

        fsi.EvalInteraction code
        fsi.EvalInteraction "open NestedModule"
        fsi.EvalInteraction "let (Value y) = client.EvaluateThunk <| fun () -> let (Value y) = x in Value (y+1)" 
        fsi.EvalExpression "y" |> shouldEqual 42


    [<Test>]
    let ``09 Class Definitions`` () =
            
        let code = """
            type Cell<'T> (x : 'T) =
                let x = ref x
                member __.Value
                    with get () = !x
                    and set y = x := y
        """

        let fsi = FsiSession.Value
        fsi.EvalInteraction code
        fsi.EvalInteraction "let c = Cell<int>(41)"
        fsi.EvalInteraction "let c' = client.EvaluateThunk <| fun () -> c.Value <- c.Value + 1 ; c"
        fsi.EvalExpression "c'.Value" |> shouldEqual 42


    [<Test>]
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

        let fsi = FsiSession.Value
        fsi.EvalInteraction code
        fsi.EvalInteraction "let results = runAsync testWorkflow"
        fsi.EvalExpression "results.Length = n" |> shouldEqual true


    [<Test>]
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

        let fsi = FsiSession.Value
        fsi.EvalInteraction code
        fsi.EvalInteraction "let result = client.EvaluateThunk query"
        fsi.EvalExpression "result = query ()" |> shouldEqual true

    [<Test>]
    let ``12 Remotely deploy an actor definition`` () =
        // temporarily disable for net46 due to issue in FSCS
        if isNet46OrAbove then () else
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

        let fsi = FsiSession.Value

        fsi.EvalInteraction code
        fsi.EvalInteraction "let actorRef = deployActor (loop 0)"

        for i in 1 .. 100 do
            fsi.EvalInteraction "actorRef <-- Increment 1"

        fsi.EvalExpression "actorRef <!= GetCount" |> shouldEqual 100

    [<Test>]
    let ``13 Add reference to external library`` () =
            
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
        let errors,code = fsc.Compile [| "fsc.exe" ; "--target:library" ; sourcePath ; "-o" ; assemblyPath |] |> Async.RunSynchronously
        if code <> 0 then failwithf "Compiler error: %A" errors

        let fsi = FsiSession.Value

        fsi.AddReferences [assemblyPath]
        fsi.EvalInteraction "open StaticAssemblyTest"
        fsi.EvalExpression "client.EvaluateThunk <| fun () -> let (TestCtor (v,_)) = value in v" |> shouldEqual 42

    [<Test>]
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

        let fsi = FsiSession.Value

        fsi.LoadScript scriptPath
        fsi.EvalInteraction "open Script"
        fsi.EvalInteraction "let r = client.EvaluateThunk <| fun () -> map factorial value"
        fsi.EvalExpression "r = map factorial value" |> shouldEqual true

    [<Test>]
    let ``15 Single Interaction - Multiple executions`` () =

        let fsi = FsiSession.Value
            
        let code = """
            let cell = ref 0
            for i in 1 .. 100 do
                cell := client.EvaluateThunk <| fun () -> cell.Value + 1

            cell.Value
        """

        fsi.EvalExpression code |> shouldEqual 100

    [<Test>]
    let ``16 Cross slice inheritance`` () =

        let fsi = FsiSession.Value
            
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

    [<Test>]
    let ``17 Cross slice interfaces`` () =

        let fsi = FsiSession.Value

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

    [<Test>]
    let ``17B Interfaces inheriting interfaces`` () =
        let fsi = FsiSession.Value

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

    [<Test>]
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

        let fsi = FsiSession.Value

        fsi.EvalInteraction code
        fsi.EvalExpression "sum" |> shouldEqual 192.


    [<Test>]
    let ``19 Native dependencies`` () =
        if is64BitProcess && not runsOnMono.Value then
            let fsi = FsiSession.Value

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

            let nativeDir = "packages/testing/MathNet.Numerics.MKL.Win-x64/content/"
            let libiomp5md = nativeDir + "libiomp5md.dll"
            let mkl = nativeDir + "MathNet.Numerics.MKL.dll"

            fsi.EvalInteraction <| "client.RegisterNativeDependency " + getFullPath libiomp5md
            fsi.EvalInteraction <| "client.RegisterNativeDependency " + getFullPath mkl

            let code' = """
                let useNativeMKL () = Control.UseNativeMKL()
                client.EvaluateThunk (fun () -> useNativeMKL () ; getRandomDeterminant ())
            """

            fsi.EvalInteraction code'

    [<Test>]
    let ``20 Update earlier bindings if value changed`` () =
        let fsi = FsiSession.Value
        fsi.EvalInteraction "let array = [|1..100|]"

        fsi.EvalExpression "client.EvaluateThunk (fun () -> Array.sum array)" |> shouldEqual 5050

        fsi.EvalInteraction "array.[49] <- 0"
        fsi.EvalExpression "client.EvaluateThunk (fun () -> Array.sum array)" |> shouldEqual 5000

        fsi.EvalInteraction "array.[49] <- 50"
        fsi.EvalExpression "client.EvaluateThunk (fun () -> Array.sum array)" |> shouldEqual 5050

    [<Test>]
    let ``21 Concurrent calls to client`` () =
        let fsi = FsiSession.Value
        fsi.EvalInteraction "[|1..20|] |> Array.Parallel.map (fun i -> client.EvaluateThunk (fun () -> i * i))"

    [<Test>]
    let ``22 Simple quotation literal`` () =
        let fsi = FsiSession.Value
        fsi.EvalInteraction "client.EvaluateThunk (fun () -> <@ 1 + 1 @>)"

    [<Test>]
    let ``23 Quotation literal references fsi code`` () =
        let fsi = FsiSession.Value
        fsi.EvalInteraction "let rec f n = if n <= 1 then n else f(n-2) + f(n-1)"
        fsi.EvalInteraction "client.EvaluateThunk(fun () -> <@ f 10 @>)"

    [<Test>]
    let ``24 Parametric quotation evaluation`` () =
        let fsi = FsiSession.Value
        defineQuotationEvaluator fsi
        fsi.EvalInteraction "let incr x = eval <@ x + 1 @>"
        fsi.EvalExpression "client.EvaluateThunk(fun () -> incr 41)" |> shouldEqual 42

    [<Test>]
    let ``24 Spliced quotation evaluation`` () =
        let fsi = FsiSession.Value
        defineQuotationEvaluator fsi
        fsi.EvalInteraction """
            let rec expand n =
                if n = 0 then <@ 0 @>
                else <@ %(expand (n-1)) + 1 @>
        """

        fsi.EvalExpression "client.EvaluateThunk(fun () -> let e = expand 10 in eval <@ %e + 32 @>)" |> shouldEqual 42

    [<Test>]
    let ``25 Cross slice quotation reference`` () =
        let fsi = FsiSession.Value
        fsi.EvalInteraction "let x = client.EvaluateThunk(fun () -> 1 + 1)"
        fsi.EvalExpression "client.EvaluateThunk(fun () -> eval <@ x @>)" |> shouldEqual 2

    [<Test>]
    let ``26 Class static field pickling`` () =
        let fsi = FsiSession.Value
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


    [<Test>]
    let ``26 Multiple Large bindings in single slice`` () =
        let fsi = FsiSession.Value
        fsi.EvalInteraction """
            let x = [|1L .. 10000000L|]
            let y = [|1L .. 10000000L|]
        """

        fsi.EvalExpression "client.EvaluateThunk (fun () -> x.Length = y.Length)" |> shouldEqual true

    [<Test>]
    let ``27 Mixed mode assemblies`` () =
        if is64BitProcess && not runsOnMono.Value then
            let fsi = FsiSession.Value

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

    [<Test>]
    let ``28 Should be able to serialize new types without explicitly requesting slice compilation`` () =
        let fsi = FsiSession.Value

        fsi.EvalInteraction """
            type LatestTypeDefinition = { Value : int }

            let roundTrip (n : int) =
                let v = { Value = n }
                let p = VagabondConfig.Serializer.Pickle(box v)
                let v' = client.EvaluateThunk(fun () -> VagabondConfig.Serializer.UnPickle<obj>(p) :?> LatestTypeDefinition)
                v'.Value
"""

        fsi.EvalExpression "roundTrip 42" |> shouldEqual 42


    [<Test>]
    let ``29 Should correctly evaluate Array2D-init calls`` () =
        let fsi = FsiSession.Value

        fsi.EvalInteraction """
            let array = client.EvaluateThunk(fun () -> Array2D.init 5 5 (fun _ _ -> 0.))
        """

        fsi.EvalExpression "array.GetLength(0)" |> shouldEqual 5
        fsi.EvalExpression "array.GetLength(1)" |> shouldEqual 5

    [<Test>]
    let ``30 Test against Cecil bug #278`` () =
        // see https://github.com/jbevain/cecil/issues/278
        let fsi = FsiSession.Value

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


    [<Test>]
    let ``31 F# 4.1 Struct types supported`` () =
        let fsi = FsiSession.Value

        fsi.EvalInteraction """
        
        [<Struct>]
        type A = A1 of a:int | A2 of b:int * string
        
        [<Struct>]
        type B = { A : A ; B : struct(int * int) }
"""

        fsi.EvalExpression """client.EvaluateThunk(fun () -> { A = A2(42,"42") ; B = struct(1,2) })""" |> shouldBe (fun x -> true)