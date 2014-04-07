module internal Nessos.Vagrant.Tests.Main

    open System
    open System.IO
    open System.Threading

    open Nessos.Vagrant.Tests.ThunkServer

    // start a thunk server instance

    [<EntryPoint>]
    let main args =
        let endpoint = if args.Length > 0 then Some <| args.[0] else None
        let _ = new ThunkServer(?endpoint = endpoint)
        while true do Thread.Sleep 1000

        0