(*** hide ***)
#I "../tests/Vagrant.Tests/bin/Debug/"
(**
Compiler Services: Using the F# tokenizer
=========================================

This tutorial demonstrates how to call the F# language tokenizer. Given F# 
source code, the tokenizer generates a list of source code lines that contain
information about tokens on each line. For each token, you can get the type
of the token, exact location as well as color kind of the token (keyword, 
identifier, number, operator, etc.).

> **NOTE:** The API used below is experimental and subject to change when later versions of the nuget package are published


Creating the tokenizer
---------------------

To use the tokenizer, reference `FSharp.Compiler.Service.dll` and open the
`SourceCodeServices` namespace:
*)
#r "FSharp.Compiler.Service.dll"
open Microsoft.FSharp.Compiler.SourceCodeServices

//// Vagrant sample: thunk server
//
//#I "../tests/Vagrant.Tests/bin/Debug/"
/////
//#r "Vagrant.Tests.exe"
/////
//
//open System
//
//
//

//let x = 12
//
//
//
//open Nessos.Vagrant.Tests.ThunkServer
//
//let client = ThunkClient.InitLocal()
//
//
//// Example 1 : incremental custom types
//
//type Foo<'T> = Bar of 'T
//
//let x = client.EvaluateThunk (fun () -> Bar 42)
//
//let incr (Bar x) = Bar (x + 1)
//
//let y = client.EvaluateThunk <| fun () -> incr x
//
//let z = client.EvaluateThunk <| fun () -> let (Bar x) = x in let (Bar y) = y in Bar(x+y)
//
//let w = 1
//let w1 = client.EvaluateThunk <| fun () -> Bar(w)
//let w2 = client.EvaluateThunk <| fun () -> Bar(w1)
//
//// Example 2 : Async
//
//let runAsync (wf : Async<'T>) =
//    client.EvaluateThunk <| fun () -> Async.RunSynchronously wf
//
//let test = async {
//
//    let worker i = async {
//        do printfn "processing job #%d" i
//        return Bar (i+1)
//    }
//
//    let (Bar value) = x
//    let! results = [|1..value|] |> Array.map worker |> Async.Parallel
//
//    return results
//}
//
//let result = runAsync test
//
//client.EvaluateThunk <| fun () -> result.Length
//
//
////
////  LinqOptimizer tests
////
//
//#r "LinqOptimizer.Base.dll"
//#r "LinqOptimizer.Core.dll"
//#r "LinqOptimizer.FSharp.dll"
//
//open Nessos.LinqOptimizer.FSharp
//
//
//let nums = [|1..10000000|]
//
//let query = 
//    nums
//    |> Query.ofSeq
//    |> Query.filter (fun num -> num % 2 = 0)
//    |> Query.map (fun num -> num * num)
//    |> Query.sum
//    |> Query.compile
//
//query ()
//
//client.EvaluateThunk query