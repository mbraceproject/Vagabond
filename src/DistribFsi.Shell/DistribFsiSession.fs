namespace Nessos.DistribFsi.Shell

    open System
    open System.IO

    open Nessos.DistribFsi
    
    open Microsoft.FSharp.Compiler.Interactive.Shell

    type DistribFsiSession private () =

        static let singletonFlag = ref false
        static let acquire () =
            lock singletonFlag (fun () ->
                if !singletonFlag then
                    invalidOp "An instance of DistribFsi is already running."
                else
                    singletonFlag := true)

        static let initInteractionCompiler (session : FsiEvaluationSession) assemblyOutDir serializerFactory =
            let serializerFactory = match serializerFactory with None -> new FsPicklerSerializerFactory() :> ISerializerFactory | Some f -> f
            let assemblyOutDir = defaultArg assemblyOutDir <| Path.GetTempPath()

            if not <| Directory.Exists assemblyOutDir then
                invalidArg "assemblyOutDir" <| sprintf "Invalid directory '%s'." assemblyOutDir

            do acquire ()
            let compiler = new InteractionCompiler(assemblyOutDir, true, "FSI-ASSEMBLY", serializerFactory)
            session.InteractionASTs.Add(fun (ast,tast,references) -> compiler.EnqueueInteraction(ast,tast,references))
            distribFsi.InstallInteractionCompiler <| compiler.GetCompilerInfo()

        static member Create(fsiConfig, argv : string [], ?inReader:TextReader, ?outWriter:TextWriter, ?errorWriter: TextWriter, ?assemblyOutDir, ?serializerFactory) =
            let inReader = defaultArg inReader Console.In
            let outWriter = defaultArg outWriter Console.Out
            let errorWriter = defaultArg errorWriter Console.Error

            let fsiSession = new FsiEvaluationSession(fsiConfig, argv, inReader, outWriter, errorWriter)
            initInteractionCompiler fsiSession assemblyOutDir serializerFactory
            fsiSession

        static member CreateDefaultGuiSession(argv : string [], ?assemblyOutDir, ?serializerFactory) =
            let fsiSession = Microsoft.FSharp.Compiler.Interactive.Main.initGuiEvaluationSession argv
            initInteractionCompiler fsiSession assemblyOutDir serializerFactory
            fsiSession