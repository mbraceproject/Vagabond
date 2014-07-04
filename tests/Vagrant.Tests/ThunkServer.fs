module Nessos.Vagrant.Tests.ThunkServer

    open System
    open System.IO
    open System.Reflection
    open System.Diagnostics

    open Microsoft.FSharp.Control

    open Nessos.Vagrant
    open Nessos.Vagrant.Tests.TcpActor

    let private defaultConnectionString = "127.0.0.1:38979"

    let private vagrant =
        let cacheDir = Path.Combine(Path.GetTempPath(), sprintf "thunkServerCache-%O" <| Guid.NewGuid())
        let _ = Directory.CreateDirectory cacheDir
        new Vagrant(cacheDirectory = cacheDir)

    type private ServerMsg =
        | GetAssemblyLoadState of AssemblyId list * AsyncReplyChannel<AssemblyLoadInfo list>
        | LoadAssemblies of PortableAssembly list * AsyncReplyChannel<AssemblyLoadInfo list>
        | EvaluteThunk of Type * (unit -> obj) * AsyncReplyChannel<Choice<obj, exn>>

    type ThunkServer (?endpoint : string) =
        
        let endpoint = defaultArg endpoint defaultConnectionString
        
        let rec serverLoop (inbox : MailboxProcessor<ServerMsg>) = async {
            let! msg = inbox.Receive()

            match msg with
            | GetAssemblyLoadState (ids, rc) ->
                let replies = ids |> List.map vagrant.GetAssemblyLoadInfo
                rc.Reply replies

            | LoadAssemblies (pas, rc) ->
                let replies = vagrant.LoadPortableAssemblies pas
                rc.Reply replies

            | EvaluteThunk (ty, f, rc) ->
                printfn "Evaluating thunk of type '%O'" ty
                let result = 
                    try 
                        let o = f () 
                        printfn "Got result: %A" o
                        Choice1Of2 o
                    with e -> 
                        printfn "Execution failed with: %A" e
                        Choice2Of2 e

                rc.Reply result

            return! serverLoop inbox
        }

        let actor = TcpActor.Create(serverLoop, endpoint, vagrant.Pickler)

        do printfn "ThunkServer(TM) listening at %s\n" endpoint

        member __.Stop = actor.Stop()


    type ThunkClient internal (?serverEndPoint : string, ?proc : Process) =

        static do TcpActor.SetDefaultPickler(vagrant.Pickler)

        let serverEndPoint = defaultArg serverEndPoint defaultConnectionString
        let client = TcpActor.Connect<ServerMsg>(serverEndPoint)

        let assemblyUploader =
            {
                new IRemoteAssemblyReceiver with
                    member __.GetLoadedAssemblyInfo(ids : AssemblyId list) = client.PostAndReplyAsync(fun ch -> GetAssemblyLoadState(ids, ch))
                    member __.PushAssemblies(pas : PortableAssembly list) = client.PostAndReplyAsync(fun ch -> LoadAssemblies(pas,ch))
            }

        member __.UploadDependenciesAsync (obj : obj) = async {

            let! errors = vagrant.SubmitObjectDependencies(assemblyUploader, obj, permitCompilation = true)
            return ()
        }

        member __.UploadDependencies (obj:obj) = __.UploadDependenciesAsync obj |> Async.RunSynchronously

        member __.EvaluateThunkAsync (f : unit -> 'T) = async {

            // load dependencies to server
            do! __.UploadDependenciesAsync f

            // evaluate thunk
            let! result = client.PostAndReplyAsync(fun ch -> EvaluteThunk(typeof<'T>, (fun () -> f () :> obj), ch))

            return
                match result with
                | Choice1Of2 o -> o :?> 'T
                | Choice2Of2 e -> raise e
        }

        member __.EvaluateThunk (f : unit -> 'T) = __.EvaluateThunkAsync f |> Async.RunSynchronously
        member __.EvaluateDelegate (f : Func<'T>) = __.EvaluateThunk f.Invoke
        member __.Kill() = proc |> Option.iter (fun p -> p.Kill())

        static member InitLocal(?serverExecutable, ?serverEndPoint) =
            let serverEndPoint = defaultArg serverEndPoint defaultConnectionString
            let executable =
                match serverExecutable with
                | None -> Assembly.GetExecutingAssembly().Location
                | Some e -> e
            
            let proc = System.Diagnostics.Process.Start(executable, serverEndPoint)
            do System.Threading.Thread.Sleep(1000)
            new ThunkClient(serverEndPoint, proc)

        static member Connect(?serverEndPoint : string) = new ThunkClient(?serverEndPoint = serverEndPoint)