namespace ThunkServer

open System
open System.IO
open System.Reflection
open System.Diagnostics
open System.Threading.Tasks

open Nessos.Thespian
open Nessos.Thespian.Remote

open Nessos.Vagabond

type internal ServerMsg =
    | GetAssemblyLoadState of AssemblyId list * IReplyChannel<AssemblyLoadInfo list>
    | LoadAssemblies of VagabondAssembly list * IReplyChannel<AssemblyLoadInfo list>
    | EvaluteThunk of Type * (unit -> obj) * IReplyChannel<Choice<obj, exn>>

type ThunkServer private () =
        
    let rec serverLoop (self : Actor<ServerMsg>) = async {
        let! msg = self.Receive()

        match msg with
        | GetAssemblyLoadState (ids, rc) ->
            let replies = ids |> List.map VagabondConfig.Vagabond.GetAssemblyLoadInfo
            do! rc.Reply replies

        | LoadAssemblies (vas, rc) ->
            let replies = VagabondConfig.Vagabond.LoadVagabondAssemblies vas
            do! rc.Reply replies

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

            do! rc.Reply result

        return! serverLoop self
    }

    let actor = serverLoop |> Actor.bind |> Actor.Publish

    do printfn "ThunkServer listening at %O\n" Actor.EndPoint

    member __.Stop = actor.Stop()
    member internal __.Ref = actor.Ref
    member __.Uri = ActorRef.toUri actor.Ref

    static member Start () = new ThunkServer()


type ThunkClient internal (server : ActorRef<ServerMsg>, ?proc : Process) =
    static let mutable exe = None

    let assemblyUploader =
        {
            new IRemoteAssemblyReceiver with
                member __.GetLoadedAssemblyInfo(ids : AssemblyId list) = server.PostWithReply(fun ch -> GetAssemblyLoadState(ids, ch))
                member __.PushAssemblies(vas : VagabondAssembly list) = server.PostWithReply(fun ch -> LoadAssemblies(vas,ch))
        }

    member __.UploadDependenciesAsync (obj : obj) = async {
        let! errors = VagabondConfig.Vagabond.SubmitObjectDependencies(assemblyUploader, obj, permitCompilation = true)
        return ()
    }

    member __.UploadDependencies (obj:obj) = __.UploadDependenciesAsync obj |> Async.RunSynchronously

    member __.EvaluateThunkAsync (f : unit -> 'T) = async {
        // load dependencies to server
        do! __.UploadDependenciesAsync f

        // evaluate thunk
        let! result = server.PostWithReply(fun ch -> EvaluteThunk(typeof<'T>, (fun () -> f () :> obj), ch))

        return
            match result with
            | Choice1Of2 o -> o :?> 'T
            | Choice2Of2 e -> raise e
    }

    member __.EvaluateThunk (f : unit -> 'T) = __.EvaluateThunkAsync f |> Async.RunSynchronously
    member __.EvaluateDelegate (f : Func<'T>) = __.EvaluateThunk f.Invoke
    member __.Kill() = proc |> Option.iter (fun p -> p.Kill())

    /// Gets or sets the server executable location.
    static member Executable
        with get () = match exe with None -> invalidOp "unset executable path." | Some e -> e
        and set path = 
            let path = Path.GetFullPath path
            if File.Exists path then exe <- Some path
            else raise <| FileNotFoundException(path)

    static member InitLocalAsync () = async {
        use receiver = Receiver.create<ActorRef<ServerMsg>> () |> Actor.Publish
        let! awaiter = receiver.ReceiveEvent |> Async.AwaitEvent |> Async.StartChild

        let argument = VagabondConfig.Pickler.Pickle receiver.Ref |> System.Convert.ToBase64String
        let proc = Process.Start(ThunkClient.Executable, argument)

        let! serverRef = awaiter
        return new ThunkClient(serverRef, proc)
    }
    
    static member InitLocal () = ThunkClient.InitLocalAsync() |> Async.RunSynchronously

    static member Connect(uri : string) = new ThunkClient(ActorRef.fromUri uri)