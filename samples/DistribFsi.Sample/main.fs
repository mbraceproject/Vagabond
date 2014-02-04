module internal Nessos.DistribFsi.Sample

    open System

    open Nessos.DistribFsi

    [<EntryPoint>]
    let main _ =

        try
            let argv = Environment.GetCommandLineArgs()

            let session = new DistribFsiSession(argv, System.IO.Path.GetTempPath(), Console.In, Console.Out, Console.Error)

            session.Run()
        with e -> printf "Exception by fsi.exe:\n%+A\n" e

        0