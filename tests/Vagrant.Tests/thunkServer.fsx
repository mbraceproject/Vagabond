(**
A Vagrant Demo: ThunkServer

This script offers a demo of the functionality of Vagrant, through an ad-hoc application.
ThunkServer, as its name suggests, is a server that receives and executes arbitrary thunks,
that is functions of type unit -> 'T. ThunkServer uses the Vagrant library to correctly
resolve and submit code dependencies, even if those happen to be dynamic assemblies as
is the case with F# Interactive.

The actual implementation of ThunkServer is a straightforward 100 lines of code.
Dependency resolution and exportation logic is handled transparently by Vagrant
**)

#r "bin/Debug/Vagrant.Tests.exe"
open Nessos.Vagrant.Tests.ThunkServer

// initialize & test a local instance
let client = ThunkClient.InitLocal()


// Example 1: simple, incremental interactions

let askDeepThought () = 42

let answer = client.EvaluateThunk askDeepThought

client.EvaluateThunk (fun () -> if answer = 42 then failwith "yet another mindless hitchhiker reference")

// Example 2: custom type definitions

type BinTree<'T> = Leaf | Node of 'T * BinTree<'T> * BinTree<'T>

let rec init = function 0 -> Leaf | n -> Node(n, init (n-1), init (n-1))
let rec map f = function Leaf -> Leaf | Node(a,l,r) -> Node(f a, map f l, map f r)
let rec reduce id f = function Leaf -> id | Node(a,l,r) -> f (f (reduce id f l) a) (reduce id f r)

let tree = client.EvaluateThunk <| fun () -> init 5

let tree' = client.EvaluateThunk <| fun () -> map (fun x -> 2. ** float x) tree

let sum = client.EvaluateThunk <| fun () -> reduce 1. (+) tree'


//
//Example 1 : incremental interactions
//
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
//// sample: deploy an actor
//
//
//open Microsoft.FSharp.Control
//open Nessos.Vagrant.Tests.TcpActor
//
//// takes an input, replies with an aggregate sum
//let rec loop state (inbox : MailboxProcessor<int * AsyncReplyChannel<int>>) =
//    async {
//        let! msg, rc = inbox.Receive ()
//
//        printfn "Received %d. Thanks!" msg
//
//        rc.Reply state
//
//        return! loop (state + msg) inbox
//    }
//
//let deployActor () = 
//    printfn "deploying actor..."
//    let a = TcpActor.Create(loop 0, "localhost:18979")
//    printfn "done"
//    a.GetClient()
//
//// deploy
//let actorRef = client.EvaluateThunk deployActor
//
//// define post function
//let postAndReply x = actorRef.PostAndReply <| fun ch -> x,ch
//
//// upload dependencies for post function
//client.UploadDependencies postAndReply
//
//// send messages to actor
//postAndReply 1
//postAndReply 2
//postAndReply 3
//
//
////
////  LinqOptimizer tests
////
//
//#r "bin/Release/LinqOptimizer.Base.dll"
//#r "bin/Release/LinqOptimizer.Core.dll"
//#r "bin/Release/LinqOptimizer.FSharp.dll"
//
//open Nessos.LinqOptimizer.FSharp
//
//
//let nums = [|1..100000|]
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