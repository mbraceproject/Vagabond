module internal Nessos.DistribFsi.Sample.Main

    open System
    open System.IO
    open System.Threading

    open Nessos.DistribFsi.Sample

    [<EntryPoint>]
    let main args =
        let _ = ThunkServer.Init(Seq.head args)
        while true do Thread.Sleep 1000

        0