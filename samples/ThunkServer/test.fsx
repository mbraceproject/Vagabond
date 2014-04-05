#r "bin/Debug/ThunkServer.exe"

open Nessos.Vagrant.Sample

let client = ThunkClient.Init()

// Example : incremental custom types

type Foo<'T> = Bar of 'T

let x = client.EvaluateThunk (fun () -> Bar 42)

let incr (Bar x) = Bar (x + 1)

let y = client.EvaluateThunk <| fun () -> incr x

let z = client.EvaluateThunk <| fun () -> let (Bar x) = x in let (Bar y) = y in Bar(x+y)

let w = 1
let w1 = client.EvaluateThunk <| fun () -> Bar(w)
let w2 = client.EvaluateThunk <| fun () -> Bar(w1)

// Example : Async

let runAsync (wf : Async<'T>) =
    client.EvaluateThunk <| fun () -> Async.RunSynchronously wf

let test = async {

    let worker i = async {
        do printfn "processing job #%d" i
        return Bar (i+1)
    }

    let (Bar value) = x
    let! results = [|1..value|] |> Array.map worker |> Async.Parallel

    return results
}

let result = runAsync test

client.EvaluateThunk <| fun () -> result.Length


//
//  LinqOptimizer tests
//

#r "bin/Debug/LinqOptimizer.Base.dll"
#r "bin/Debug/LinqOptimizer.Core.dll"
#r "bin/Debug/LinqOptimizer.FSharp.dll"

open LinqOptimizer.FSharp


let nums = [|1..10000000|]

let query = 
    nums
    |> Query.ofSeq
    |> Query.filter (fun num -> num % 2 = 0)
    |> Query.map (fun num -> num * num)
    |> Query.sum
    |> Query.compile

query ()

client.EvaluateThunk query