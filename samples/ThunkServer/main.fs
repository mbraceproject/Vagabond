module Nessos.DistribFsi.Sample.Main

    open System
    open System.IO
    open System.Threading

    open Nessos.DistribFsi
    open Nessos.DistribFsi.Shell
    open Nessos.DistribFsi.Sample.Server

    [<assembly: AutoOpen("Nessos.DistribFsi.Sample.Main")>]
    do()

    let initThunkServer () = ThunkClient.Init(compileCurrentInteractions = true)

    // Mark the main thread as STAThread since it is a GUI thread
    [<EntryPoint>]
    [<STAThread()>]    
    let internal main _ =

        let argv = Environment.GetCommandLineArgs()

        match argv |> Array.tryFind(fun arg -> arg.StartsWith "--thunk-server:") with
        | Some arg -> 
            // thunk server mode
            do distribFsi.InstallClientSerializer(new FsPicklerSerializer())
            let servId = arg.Split(':').[1]
            let _ = ThunkServer.Init(servId, Settings.distribFsi.Serializer)
            while true do Thread.Sleep 1000

            0

        | None ->
            // fsi mode

            // add current assembly as reference
            let argv = [| yield! argv ; yield sprintf "-r:%s" <| System.Reflection.Assembly.GetExecutingAssembly().Location |]
        
            let session = DistribFsiSession.CreateDefaultGuiSession(argv)

            session.Run()

            0