module internal Nessos.Vagrant.Sample.Main

    open System
    open System.IO
    open System.Threading

    open Nessos.Vagrant.Sample

    [<EntryPoint>]
    let main args =
        let _ = ThunkServer.Init(Seq.head args)
        while true do Thread.Sleep 1000

        0