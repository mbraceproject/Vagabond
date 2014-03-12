#r "bin/Debug/ThunkServer.exe"

open Nessos.Vagrant.Sample

let client = ThunkClient.Init()

// Example : incremental custom types

type Foo<'T> = Bar of 'T

let x = client.EvaluateThunk <| fun () -> Bar 42

let incr (Bar x) = Bar (x + 1)

let y = client.EvaluateThunk <| fun () -> incr x

let z = client.EvaluateThunk <| fun () -> let (Bar x) = x in let (Bar y) = y in Bar(x+y)

let w = 1
let w = client.EvaluateThunk <| fun () -> Bar(w)

// Example : Async

let runAsync (wf : Async<'T>) =
    client.EvaluateThunk <| fun () -> Async.RunSynchronously wf

let test = async {
    let (Bar value) = x
    let! results = [|1..value|] |> Array.map (fun i -> async { return Bar (i + 1) }) |> Async.Parallel

    return results
}

let result = runAsync test

client.EvaluateThunk <| fun () -> result.Length