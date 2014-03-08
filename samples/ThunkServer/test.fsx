#I "bin/Debug"
#r "ThunkServer.exe"

open Nessos.DistribFsi.Sample

let client = ThunkClient.Init()


type Foo<'T> = Bar of 'T


let x = client.EvaluateThunk <| fun () -> Bar 42

let incr (Bar x) = Bar (x + 1)

let y = client.EvaluateThunk <| fun () -> incr x

let z = client.EvaluateThunk <| fun () -> let (Bar x) = x in let (Bar y) = y in Bar(x+y)

let w = client.EvaluateThunk <| fun () -> Bar(z)