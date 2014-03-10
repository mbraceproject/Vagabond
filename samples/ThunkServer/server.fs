namespace Nessos.Vagrant.Sample

    open System
    open System.IO
    open System.Diagnostics
    open System.Reflection
    open System.Runtime.Remoting
    open System.Runtime.Remoting.Channels
    open System.Runtime.Remoting
    open System.Runtime.Remoting.Lifetime

    open Nessos.Vagrant

    type ThunkServer private () =
        inherit System.MarshalByRefObject()

        let lockObj = obj ()

        let distribFsiClient = CompilationClient.Create()

        member __.EvaluateThunk(pickle : byte [], assemblyLocations : string list) =
            lock lockObj (fun () ->
                // 1. load dependencies to AppDomain
                for location in assemblyLocations do
                    Assembly.LoadFrom(location) |> ignore

                // 2. now, unpickle
                let pickle = distribFsiClient.Pickler.UnPickle<Pickle>(pickle)

                // 3. load pickle data
                let ty, thunk = distribFsiClient.LoadPortablePickle pickle |> unbox<Type * (unit -> obj)>
                printfn "Evaluating thunk of type '%O'" ty
                let result = 
                    try 
                        let o = thunk () 
                        printfn "Got result: %A" o
                        Choice1Of2 o
                    with e -> 
                        printfn "Execution failed with: %A" e
                        Choice2Of2 e

                distribFsiClient.Pickler.Pickle<Choice<obj, exn>>(result))

        static member internal Init(serverId:string) =
            printf "Initializing Thunk Server (%s)... " serverId
            let chan = new Ipc.IpcChannel(serverId)
            LifetimeServices.LeaseTime            <- TimeSpan(7,0,0,0); // days,hours,mins,secs 
            LifetimeServices.LeaseManagerPollTime <- TimeSpan(7,0,0,0);
            LifetimeServices.RenewOnCallTime      <- TimeSpan(7,0,0,0);
            LifetimeServices.SponsorshipTimeout   <- TimeSpan(7,0,0,0);
            ChannelServices.RegisterChannel(chan,false);
            let server = new ThunkServer()
            let _ = RemotingServices.Marshal(server,"ThunkServer") 
            printfn "Done"

            printfn "\nGIMME YOUR THUNKS\n"

        static member internal Connect(serverId : string) =
            Activator.GetObject(typeof<ThunkServer>,"ipc://" + serverId + "/ThunkServer") :?> ThunkServer


    type ThunkClient private (serverProc : Process, server : ThunkServer) =

        let distribFsiServer = CompilationServer.CreateServer()

        member __.EvaluateThunk(f : unit -> 'T) =
            let payload = typeof<'T>, fun () -> f () :> obj
            let pickle, errors = distribFsiServer.ComputePortablePickle payload
            let data = distribFsiServer.Pickler.Pickle<Pickle> pickle
            let assemblies = pickle.DependencyInfo.AllDependencies |> List.map (fun a -> a.Location)
            let resultData = server.EvaluateThunk(data, assemblies) 
            let result = distribFsiServer.Pickler.UnPickle<Choice<obj,exn>> resultData

            match result with
            | Choice1Of2 o -> o :?> 'T
            | Choice2Of2 e -> raise e

            member __.Kill() = serverProc.Kill ()

            static member Init() =
                let serverId = Guid.NewGuid().ToString()
                let self = Assembly.GetExecutingAssembly().Location
                let proc = Process.Start(self, serverId)
                do System.Threading.Thread.Sleep(1000)
                let server = ThunkServer.Connect(serverId)
                new ThunkClient(proc, server)
