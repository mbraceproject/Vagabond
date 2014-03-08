#I "bin/Debug"
#r "ThunkServer.exe"

open Nessos.DistribFsi.Sample

let client = ThunkClient.Init()


type Foo = Bar of int

let incr (Bar x) = Bar (x + 1)

let x = client.EvaluateThunk <| fun () -> Bar 42
let x = client.EvaluateThunk <| fun () -> incr x