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

#I "bin/Release/"

#r "FsPickler.dll"
#r "Vagrant.Tests.exe"
open Nessos.Vagrant.Tests.ThunkServer

// initialize & test a local instance
let client = ThunkClient.InitLocal()

(**
Example 1: simple, incremental interactions
**)

let askDeepThought () = 42

let answer = client.EvaluateThunk askDeepThought

client.EvaluateThunk (fun () -> if answer = 42 then failwith "yet another mindless hitchhiker reference")

(**
Example 2: custom type definitions
**)

type BinTree<'T> = Leaf | Node of 'T * BinTree<'T> * BinTree<'T>

let rec init = function 0 -> Leaf | n -> Node(n, init (n-1), init (n-1))
let rec map f = function Leaf -> Leaf | Node(a,l,r) -> Node(f a, map f l, map f r)
let rec reduce id f = function Leaf -> id | Node(a,l,r) -> f (f (reduce id f l) a) (reduce id f r)

let tree = client.EvaluateThunk <| fun () -> init 5

let tree' = client.EvaluateThunk <| fun () -> map (fun x -> 2. ** float x) tree

let sum = client.EvaluateThunk <| fun () -> reduce 1. (+) tree'

(**
Example 3: Type providers
**)

#r "../../packages/FSharp.Data.2.0.8/lib/net40/FSharp.Data.dll"

open FSharp.Data

// World Bank

let wb = WorldBankData.GetDataContext()

let top5 () = 
    query {
        for country in wb.Regions.``Euro area``.Countries do
        sortByDescending country.Indicators.``GDP per capita (current US$)``.[2012]
        take 5
        select country.Name
    } |> Seq.toList

client.EvaluateThunk top5

// FreeBase

let fb = FSharp.Data.FreebaseData.GetDataContext()

let displaySussman () =
    let sussman = fb.``Science and Technology``.Computers.``Computer Scientists``.Individuals.``Gerald Jay Sussman``
    for line in sussman.Blurb do printfn "%s" line

client.EvaluateThunk displaySussman
        

(**
Example 4 : Asynchronous workflows
**)

let runRemoteAsync (workflow : Async<'T>) =
    client.EvaluateThunk(fun () -> Async.RunSynchronously workflow)

let test = async {
    
    let printfn fmt = Printf.ksprintf System.Console.WriteLine fmt
    let workflow i = async { if i = 7 then invalidOp "error" else printfn "Running task %d.." i }

    try
        let! results = [1..10] |> List.map workflow |> Async.Parallel
        return None

    with :? System.InvalidOperationException as e ->
        return Some "error"
}

runRemoteAsync test

(**
Example 5: Deploy a locally defined actor
**)

open Microsoft.FSharp.Control
open Nessos.Vagrant.Tests.TcpActor

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
let rec actorRef = client.EvaluateThunk deployActor
and postAndReply x = actorRef.PostAndReply <| fun ch -> x,ch

// send messages
postAndReply 1
postAndReply 2
postAndReply 3

(*
Example 6: Deploy library-generated dynamic assemblies
*)

#I "../../packages/LinqOptimizer.FSharp.0.6.2/lib/"

#r "LinqOptimizer.Base.dll"
#r "LinqOptimizer.Core.dll"
#r "LinqOptimizer.FSharp.dll"

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