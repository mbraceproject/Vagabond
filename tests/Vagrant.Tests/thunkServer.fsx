#r "bin/Debug/FsPickler.dll"
#r "bin/Debug/Vagrant.Tests.exe"

open Nessos.Vagrant.Tests.ThunkServer

let client = ThunkClient.InitLocal()

// Example 1 : incremental custom types

type Foo<'T> = Bar of 'T

let x = client.EvaluateThunk (fun () -> Bar 42)

let incr (Bar x) = Bar (x + 1)

let y = client.EvaluateThunk <| fun () -> incr x

let z = client.EvaluateThunk <| fun () -> let (Bar x) = x in let (Bar y) = y in Bar(x+y)

let w = 1
let w1 = client.EvaluateThunk <| fun () -> Bar(w)
let w2 = client.EvaluateThunk <| fun () -> Bar(w1)

// Example 2 : Async

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

// sample: deploy an actor


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

// deploy
let actorRef = client.EvaluateThunk deployActor

// define post function
let postAndReply x = actorRef.PostAndReply <| fun ch -> x,ch

// upload dependencies for post function
client.UploadDependencies postAndReply

// send messages to actor
postAndReply 1
postAndReply 2
postAndReply 3


//
//  LinqOptimizer tests
//

#r "bin/Release/LinqOptimizer.Base.dll"
#r "bin/Release/LinqOptimizer.Core.dll"
#r "bin/Release/LinqOptimizer.FSharp.dll"

open Nessos.LinqOptimizer.FSharp


let nums = [|1..100000|]

let query = 
    nums
    |> Query.ofSeq
    |> Query.filter (fun num -> num % 2 = 0)
    |> Query.map (fun num -> num * num)
    |> Query.sum
    |> Query.compile

query ()

client.EvaluateThunk query