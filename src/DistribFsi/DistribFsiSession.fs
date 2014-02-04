namespace Nessos.DistribFsi

    open System
    open System.IO

    open Microsoft.FSharp.Compiler.Interactive.Main

    type DistribFsiSession (argv : string [], assemblyOutDir : string, inReader:TextReader, outWriter:TextWriter, errorWriter: TextWriter) =

        static let singletonFlag = ref false
        do
            lock singletonFlag (fun () -> 
                if !singletonFlag then
                    invalidOp "An instance of DistribFsi is already running."
                else
                    singletonFlag := true)

            if not <| Directory.Exists assemblyOutDir then
                invalidArg "assemblyOutDir" <| sprintf "Invalid directory '%s'." assemblyOutDir

        let fsiSession = prepareFsiInteractiveSession(argv, inReader, outWriter, errorWriter)
        let compiler = new InteractionCompiler("/mbrace", true, "FSI-ASSEMBLY")
        do
            match fsiSession.PublishedASTs with
            | None -> invalidOp "internal error"
            | Some e -> e.Add compiler.EnqueueInteraction |> ignore

            DistribFsiRegistry.InstallInteractionCompiler <| compiler.GetCompilerInfo()

        // TODO: expose more of the fsiSession api
        member __.Run () = fsiSession.Run ()