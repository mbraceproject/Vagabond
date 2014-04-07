module Nessos.Vagrant.Tests.Setup

    open Nessos.Vagrant

    open Microsoft.FSharp.Compiler.Interactive.Shell
    open Microsoft.FSharp.Compiler.ErrorLogger

    open System
    open System.IO

    open NUnit

    type FsiEvaluationSession with
        
        member fsi.AddReference (path : string) =
            let directive = sprintf """#r "%s" """ path
            fsi.EvalInteraction directive

    let initInteractionServer () =
        let inStream = new StringReader("")
        let outStream = new StringWriter()
//        let errStream = new StringWriter()

        let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()

        new FsiEvaluationSession(fsiConfig, [| "fsi.exe" ; "--noninteractive" |], inStream, outStream, System.Console.Error)


    let fsi = initInteractionServer ()

    let loadThunkServer (fsi : FsiEvaluationSession) =

//        let localPath = Path.Combine(Directory.GetCurrentDirectory() , "bin", "Debug")
        let localPath = Path.Combine(__SOURCE_DIRECTORY__, "bin", "Debug")

        let addRef path = sprintf """#r @"%s" """ path
        let addLocalRef name =
            let path = Path.Combine(localPath, name)
            addRef path

        let lines =
            [
                addLocalRef "FsPickler.dll"
                addLocalRef "Mono.Cecil.dll"
                addLocalRef "Mono.Reflection.dll"
                addLocalRef "Vagrant.dll"
                addLocalRef "ThunkServer.exe"

                "open Nessos.Vagrant.Sample"
                "let client = ThunkClient.Init()"
            ]

        lines |> String.concat "\r\n" |> fsi.EvalInteraction


    loadThunkServer fsi

    fsi.EvalExpression("client.EvaluateThunk <| fun () -> 42")

//    fsi.EvalExpression("client.EvaluateThunk <| fun () -> 42")
////
////    let foo = 
////        try fsi.EvalExpression("client.E") |> Choice1Of2
////        with e ->
////            match e.InnerException with
////            | StopProcessing -> Choice2Of2 e
////            | _ -> reraise()
//
//    let ms = foo.Value.ReflectionType.GetMethods()