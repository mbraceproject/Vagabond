module internal ThunkServer.Main

open System
open System.IO
open System.Threading

open Nessos.Thespian

// start a thunk server instance

[<EntryPoint>]
let main args =

    let receiver =
        if args.Length > 0 then 
            let bytes = args.[0] |> System.Convert.FromBase64String 
            VagabondConfig.Serializer.UnPickle<ActorRef<ActorRef<ServerMsg>>> bytes |> Some
        else
            None

    let server = ThunkServer.Start()
    receiver |> Option.iter (fun r -> r.Post server.Ref)
    while true do Thread.Sleep 1000
    0