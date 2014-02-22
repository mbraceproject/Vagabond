namespace Nessos.DistribFsi.Sample


    type Thunk<'T> = Func of (unit -> 'T)
    with
        member x.Value = let (Func f) = x in f

    module Server =

        open System
        open System.IO
        open System.Diagnostics
        open System.Reflection
        open System.Runtime.Remoting
        open System.Runtime.Remoting.Channels
        open System.Runtime.Remoting
        open System.Runtime.Remoting.Lifetime

        open Nessos.DistribFsi

        let internal pickle<'T> (serializer : ISerializer) (x : 'T) =
            use m = new MemoryStream()
            serializer.Serialize<'T> m x
            m.ToArray()

        let internal unpickle<'T> (serializer : ISerializer) (data : byte []) =
            use m = new MemoryStream(data)
            serializer.Deserialize<'T> m

        type ThunkServer private (serializer : ISerializer) =
            inherit System.MarshalByRefObject()

            let lockObj = obj ()

            member __.LoadAssemlies(locations:string list) = 
                for location in locations do
                    Assembly.LoadFrom(location) |> ignore

            member __.EvaluateThunk(data : byte []) =
                lock lockObj (fun () ->
                    let ty, thunk = unpickle<Type * (unit -> obj)> serializer data
                    printfn "Evaluating thunk of type '%O'" ty
                    let result = 
                        try 
                            let o = thunk () 
                            printfn "Got result: %A" o
                            Choice1Of2 o
                        with e -> 
                            printfn "Execution failed with: %A" e
                            Choice2Of2 e

                    pickle serializer result)

            static member internal Init(serverId:string, serializer:ISerializer) =
                printf "Initializing Thunk Server (%s)... " serverId
                let chan = new Ipc.IpcChannel(serverId)
                LifetimeServices.LeaseTime            <- TimeSpan(7,0,0,0); // days,hours,mins,secs 
                LifetimeServices.LeaseManagerPollTime <- TimeSpan(7,0,0,0);
                LifetimeServices.RenewOnCallTime      <- TimeSpan(7,0,0,0);
                LifetimeServices.SponsorshipTimeout   <- TimeSpan(7,0,0,0);
                ChannelServices.RegisterChannel(chan,false);
                let server = new ThunkServer(serializer)
                let _ = RemotingServices.Marshal(server,"ThunkServer") 
                printfn "Done"

                printfn "\nGIMME YOUR THUNKS\n"

            static member internal Connect(serverId : string) =
                Activator.GetObject(typeof<ThunkServer>,"ipc://" + serverId + "/ThunkServer") :?> ThunkServer


        type ThunkClient private (serverProc : Process, server : ThunkServer, serializer : ISerializer, compileCurrentIntaction) =

            member __.EvaluateThunk(f : Thunk<'T>) =
                // dependency resolution logic
                let info = distribFsi.GetObjectDependencies f.Value
                let info =
                    if info.RequiresAssemblyCompilation then
                        if info.DependsOnCurrentInteraction && not compileCurrentIntaction then
                            failwith "Thunk depends on currently executing interaction; will not compile."
                        let compiledAssembly = distribFsi.RequestCompilation(compilePendingInteraction = compileCurrentIntaction)
                        // recompute object dependencies
                        distribFsi.GetObjectDependencies f
                    else
                        // no compilation required, leave as is
                        info

                // filter gac assemblies
                let assemblies = 
                    info.Dependencies.Value 
                    |> List.filter(fun a -> not a.GlobalAssemblyCache)
                    |> List.map (fun a -> a.Location)

                // pass to server for loading
                do server.LoadAssemlies assemblies

                let pickledThunk = pickle serializer (typeof<'T>, fun () -> f.Value () :> obj)
                let pickledResult = server.EvaluateThunk pickledThunk
                match unpickle<Choice<obj,exn>> serializer pickledResult with
                | Choice1Of2 o -> o :?> 'T
                | Choice2Of2 e -> raise e

            member __.Kill() = serverProc.Kill ()

            static member Init(?compileCurrentInteractions) =
                let compileCurrentInteractions = defaultArg compileCurrentInteractions true
                let serverId = Guid.NewGuid().ToString()
                let serializer = distribFsi.Serializer
                let self = Assembly.GetExecutingAssembly().Location
                let proc = Process.Start(self, sprintf "--thunk-server:%s" serverId)
                do System.Threading.Thread.Sleep(1000)
                let server = ThunkServer.Connect(serverId)
                new ThunkClient(proc, server, serializer, compileCurrentInteractions)
