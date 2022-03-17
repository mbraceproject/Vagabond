namespace ThunkServer

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Diagnostics
open System.Threading.Tasks

open Nessos.Thespian
open Nessos.Thespian.Remote

open MBrace.Vagabond
open MBrace.Vagabond.AssemblyProtocols
open MBrace.Vagabond.ExportableAssembly

#nowarn "1571"

type internal ServerMsg =
    | GetAssemblyLoadState of AssemblyId [] * IReplyChannel<AssemblyLoadInfo []>
    | LoadAssemblies of ExportableAssembly [] * IReplyChannel<AssemblyLoadInfo []>
    | EvaluteThunk of Type * (unit -> obj) * IReplyChannel<Choice<obj, exn>>

[<AutoSerializable(false)>]
type ThunkServer private () =
        
    let rec serverLoop (self : Actor<ServerMsg>) = async {
        let! msg = self.Receive()

        match msg with
        | GetAssemblyLoadState (ids, rc) ->
            let replies = VagabondConfig.Instance.GetAssemblyLoadInfo ids
            do! rc.Reply replies

        | LoadAssemblies (rvas, rc) ->
            let vas = VagabondConfig.Instance.CacheRawAssemblies rvas
            let replies = VagabondConfig.Instance.LoadVagabondAssemblies(vas)
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

    member __.Stop () = actor.Stop()
    member internal __.Ref = actor.Ref
    member __.Uri = ActorRef.toUri actor.Ref

    static member Start () = new ThunkServer()


/// Client object for interacting with thunk server
[<AutoSerializable(false)>]
type ThunkClient internal (server : ActorRef<ServerMsg>, ?proc : Process) =
    static let mutable exe = None
    static let isWindowsPlatform = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

    let assemblyUploader =
        {
            new IRemoteAssemblyReceiver with
                member __.GetLoadedAssemblyInfo(ids : AssemblyId []) = server.PostWithReply(fun ch -> GetAssemblyLoadState(ids, ch))
                member __.PushAssemblies(vas : VagabondAssembly []) = async {
                    let rvas = VagabondConfig.Instance.CreateRawAssemblies(vas)
                    return! server.PostWithReply(fun ch -> LoadAssemblies(rvas,ch))
                }
        }

    /// Upload dependencies for object graph to thunk server
    member __.UploadDependenciesAsync (obj : obj) = async {
        let! errors = VagabondConfig.Instance.SubmitObjectDependencies(assemblyUploader, obj, permitCompilation = true)
        return ()
    }

    /// Upload dependencies for object graph to thunk server
    member __.UploadDependencies (obj:obj) = __.UploadDependenciesAsync obj |> Async.RunSynchronously

    /// Asynchronously evaluates a thunk on remote server
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

    /// Evaluates a thunk on remote server
    member __.EvaluateThunk (f : unit -> 'T) = __.EvaluateThunkAsync f |> Async.RunSynchronously
    /// Evaluates a function on remote server
    member __.EvaluateDelegate (f : Func<'T>) = __.EvaluateThunk f.Invoke
    /// Evaluates a function on remote server
    member __.EvaluateDelegate (f : Action) = __.EvaluateThunk f.Invoke
    /// Kills thunk server if local process
    member __.Kill() = proc |> Option.iter (fun p -> p.Kill())
    /// Register a native assembly for instance
    member __.RegisterNativeDependency(path : string) = VagabondConfig.Instance.RegisterNativeDependency path |> ignore
    /// Gets registered native assemblies for instance
    member __.NativeAssemblies = VagabondConfig.Instance.NativeDependencies |> Array.map (fun d -> d.Image)

    /// Gets or sets the server executable location.
    static member Executable
        with get () = match exe with None -> invalidOp "unset executable path." | Some e -> e
        and set path = 
            let path = Path.GetFullPath path
            if File.Exists path then exe <- Some path
            elif isWindowsPlatform && File.Exists (path + ".exe") then exe <- Some (path + ".exe")
            else raise <| FileNotFoundException(path)

    static member InitLocalAsync () = async {
        use receiver = Receiver.create<ActorRef<ServerMsg>> () |> Actor.Publish
        let! awaiter = receiver.ReceiveEvent |> Async.AwaitEvent |> Async.StartChild

        let argument = VagabondConfig.Serializer.Pickle receiver.Ref |> System.Convert.ToBase64String
        let proc = Process.Start(ThunkClient.Executable, argument + " --roll-forward Major")

        let! serverRef = awaiter
        return new ThunkClient(serverRef, proc)
    }
    
    /// Initialize a local thunk server
    static member InitLocal () = ThunkClient.InitLocalAsync() |> Async.RunSynchronously

    /// Conect to a thunk server of given thespian uri
    static member Connect(uri : string) = new ThunkClient(ActorRef.fromUri uri)
