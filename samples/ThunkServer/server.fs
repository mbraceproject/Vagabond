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

        let client = new VagrantClient()

        member __.LoadAssemblies(data : byte []) =
            // BinaryFormatter - the default .NET remoting serializer will not work correctly in some cases
            // explicitly pickle the data instead
            let assemblies = client.Pickler.UnPickle<PortableAssembly list>(data)
            let response = client.LoadPortableAssemblies(assemblies)
            client.Pickler.Pickle response

        member __.EvaluateThunk(bytes : byte []) =
            // BinaryFormatter - the default .NET remoting serializer will not work correctly in some cases
            // explicitly pickle the data instead
            lock lockObj (fun () ->
                
                // unpickle payload
                let ty, thunk = client.Pickler.UnPickle<Type * (unit -> obj)>(bytes)

                printfn "Evaluating thunk of type '%O'" ty
                let result = 
                    try 
                        let o = thunk () 
                        printfn "Got result: %A" o
                        Choice1Of2 o
                    with e -> 
                        printfn "Execution failed with: %A" e
                        Choice2Of2 e

                client.Pickler.Pickle<Choice<obj, exn>>(result))

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

        static let vagrant = new VagrantServer()

        member __.EvaluateThunk(f : unit -> 'T) =
            let submitAssemblies (pas : PortableAssembly list) = async {
                let pickle = vagrant.Pickler.Pickle(pas)
                let response = server.LoadAssemblies(pickle)
                return vagrant.Pickler.UnPickle<AssemblyLoadResponse list>(response)
            }

            // submit dependencies to thunkServer
            let errors = vagrant.SubmitObjectDependencies(submitAssemblies, f, permitCompilation = true) |> Async.RunSynchronously

            // evaluate thunk
            let payload = typeof<'T>, fun () -> f () :> obj
            let data = vagrant.Pickler.Pickle<Type * (unit -> obj)> payload

            let reply = server.EvaluateThunk data

            let result = vagrant.Pickler.UnPickle<Choice<obj,exn>> reply
            match result with
            | Choice1Of2 o -> o :?> 'T
            | Choice2Of2 e -> raise e

        member __.EvaluateDelegate(f : Func<'T>) = __.EvaluateThunk(f.Invoke)

        member __.Kill() = serverProc.Kill ()

        static member Init() =
            let serverId = Guid.NewGuid().ToString()
            let self = Assembly.GetExecutingAssembly().Location
            let proc = Process.Start(self, serverId)
            do System.Threading.Thread.Sleep(1000)
            let server = ThunkServer.Connect(serverId)
            new ThunkClient(proc, server)