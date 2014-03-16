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

    type Data = DynamicAssemblySlice list * Type * (unit -> obj)

    type ThunkServer private () =
        inherit System.MarshalByRefObject()

        let lockObj = obj ()

        let client = new VagrantClient()

        member __.EvaluateThunk(assemblyLocations : string list, bytes : byte []) =
            lock lockObj (fun () ->
                // 1. load dependencies to AppDomain
                for location in assemblyLocations do
                    Assembly.LoadFrom(location) |> ignore

                // 2. unpickle payload
                let slices, ty, thunk = client.Pickler.UnPickle<Data>(bytes)

                // 3. load assembly dependency info
                let errors = client.LoadTypeInitializers slices

                // 4. evaluate
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

        static let lastFsiSlice : DynamicAssemblySlice option ref = ref None

        static let tryFindLatestFsiSlice(slices : DynamicAssemblySlice list) =
            match Utilities.TryGetFsiDynamicAssembly() with
            | None -> None
            | Some a ->
                slices |> List.tryFind(fun s -> s.DynamicAssemblyQualifiedName = a.FullName)

        static let getLatestSliceUpdate () =
            match !lastFsiSlice with
            | None -> None
            | Some slice ->
                // TODO : check scripts!
                let currentInteraction = Utilities.TryGetLatestFsiInteraction().Value
                match vagrant.TryGetSliceOfType currentInteraction with
                | Some sl when sl.SliceId = slice.SliceId ->
                    // slice pending execution, do not erase yet
                    ()
                | _ ->
                    // slice has completed execution, remove from temporary state
                    lastFsiSlice := None

                // regenerate type initializers for the asssembly
                Some <| vagrant.GetSliceInfo(slice.Assembly, generateTypeInitializers = true)


        member __.EvaluateThunk(f : unit -> 'T) =
            // get values from previous slice if necessary
            let previous = getLatestSliceUpdate ()
            // traverse dependencies & compile if necessary
            let newSlices, dependencies = vagrant.ComputeObjectDependencies(f, permitCompilation = true, generateTypeInitializers = true)
            // record latest fsi slice, if such a slice exists
            do tryFindLatestFsiSlice newSlices |> Option.iter (fun sl -> lastFsiSlice := Some sl)

            let data = vagrant.Pickler.Pickle<Data>((Option.toList previous @ newSlices, typeof<'T>, fun () -> f () :> obj))
            let assemblyPaths = 
                dependencies 
                |> List.choose (fun a -> if a.GlobalAssemblyCache then None else Some a.Location)

            let resultData = server.EvaluateThunk(assemblyPaths, data)
            let result = vagrant.Pickler.UnPickle<Choice<obj,exn>> resultData
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