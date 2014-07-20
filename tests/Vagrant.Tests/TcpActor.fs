module Nessos.Vagrant.Tests.TcpActor

    // mini distributed actor implementation

    open System
    open System.Net
    open System.Net.Sockets
    open System.IO
    open System.Threading
    open System.Threading.Tasks

    open Microsoft.FSharp.Control

    open Nessos.FsPickler

    type AsyncBuilder with
        member __.Bind(f : Task<'T>, g : 'T -> Async<'S>) = __.Bind(Async.AwaitTask f, g)
        member __.Bind(f : Task, g : unit -> Async<'S>) = __.Bind(f.ContinueWith ignore, g)

    type Stream with
        member s.AsyncWriteBytes (bytes : byte []) =
            async {
                do! s.WriteAsync(BitConverter.GetBytes bytes.Length, 0, 4)
                do! s.WriteAsync(bytes, 0, bytes.Length)
                do! s.FlushAsync()
            }

        member s.AsyncReadBytes(length : int) =
            let rec readSegment buf offset remaining =
                async {
                    let! read = s.ReadAsync(buf, offset, remaining)
                    if read < remaining then
                        return! readSegment buf (offset + read) (remaining - read)
                    else
                        return ()
                }

            async {
                let bytes = Array.zeroCreate<byte> length
                do! readSegment bytes 0 length
                return bytes
            }

        member s.AsyncReadBytes() =
            async {
                let! lengthArr = s.AsyncReadBytes 4
                let length = BitConverter.ToInt32(lengthArr, 0)
                return! s.AsyncReadBytes length
            }

    type private PicklerRegistry () =
        static let container = ref None

        static member SetDefaultPickler(p : FsPicklerSerializer, ?overwrite) =
            let overwrite = defaultArg overwrite false
            lock container (fun () ->
                match !container with
                | Some _ when not overwrite -> invalidOp "A default pickler has already been registered."
                | _ -> container := Some p)

        static member DefaultPickler = match !container with Some p -> p | None -> FsPickler.CreateBinary() :> _

    // existentially pack reply channels

    type private IReplyChannelContainer<'T> =
        abstract PostWithReply : MailboxProcessor<'T> -> Async<obj>

    and private ReplyChannelContainer<'T, 'R>(msgB : AsyncReplyChannel<'R> -> 'T) =
        interface IReplyChannelContainer<'T> with
            member __.PostWithReply (mb : MailboxProcessor<'T>) = async {
                let! r = mb.PostAndAsyncReply msgB
                return r :> obj
            }

    type private ServerRequest<'T> =
        | Post of 'T
        | PostWithReply of IReplyChannelContainer<'T>

    and private ServerResponse =
        | Acknowledge
        | Reply of obj
        | Fault of exn

        
    type TcpActor<'T> (mailbox : MailboxProcessor<'T>, endpoint : IPEndPoint, ?pickler : FsPicklerSerializer) =

        let pickler = match pickler with None -> PicklerRegistry.DefaultPickler | Some p -> p
        let listener = new TcpListener(endpoint)

        let rec serverLoop () = async {
            try
                let! (client : TcpClient) = listener.AcceptTcpClientAsync()

                use client = client
                use stream = client.GetStream()

                let! (bytes : byte []) = stream.AsyncReadBytes()

                let! response = async {
                    try
                        let request = pickler.UnPickle<ServerRequest<'T>> bytes

                        match request with
                        | Post msg -> 
                            do mailbox.Post msg
                            return Acknowledge
                        | PostWithReply rcc ->
                            let! reply = rcc.PostWithReply mailbox
                            return Reply reply
                    with e -> return Fault e
                }

                let bytes = pickler.Pickle response
                do! stream.AsyncWriteBytes bytes

            with e -> printfn "Server error: %A" e

            return! serverLoop ()
        }

        let cts = new CancellationTokenSource()
        do 
            listener.Start()
            Async.Start(serverLoop (), cts.Token)

        member __.Stop () = cts.Cancel() ; listener.Stop()
        member __.MailboxProcessor = mailbox
        member __.EndPoint = endpoint
        member __.GetClient () = new TcpActorClient<'T>(endpoint, pickler)

    
    and 
        [<CustomPickler>]
        TcpActorClient<'T>(serverEndpoint : IPEndPoint, ?pickler : FsPicklerSerializer) =
        
        let pickler = match pickler with None -> PicklerRegistry.DefaultPickler | Some p -> p

        let sendRequest (request : ServerRequest<'T>) = async {
            use client = new TcpClient()
            do! client.ConnectAsync(serverEndpoint.Address, serverEndpoint.Port)
            use stream = client.GetStream()

            let bytes = pickler.Pickle request
            do! stream.AsyncWriteBytes bytes
            let! (reply : byte []) = stream.AsyncReadBytes()

            return pickler.UnPickle<ServerResponse> reply
        }

        member __.PostAsync (msg : 'T) = async {
            let! response = sendRequest <| Post msg
            match response with
            | Acknowledge -> return ()
            | Reply o -> return failwithf "TcpActor: invalid response '%O'." o
            | Fault e -> return raise e
        }

        member __.PostAndReplyAsync (msgB : AsyncReplyChannel<'R> -> 'T) = async {
            let wrapper = new ReplyChannelContainer<'T,'R>(msgB)
            let! response = sendRequest <| PostWithReply wrapper
            match response with
            | Reply (:? 'R as r) -> return r
            | Reply o -> return failwithf "TcpActor: invalid response '%O'." o
            | Acknowledge -> return failwith "TcpActor: invalid response."
            | Fault e -> return raise e
        }

        member __.Post msg = __.PostAsync msg |> Async.RunSynchronously
        member __.PostAndReply msgB = __.PostAndReplyAsync msgB |> Async.RunSynchronously
        member __.ServerEndPoint = serverEndpoint

        static member CreatePickler(r : IPicklerResolver) =
            let epp = r.Resolve<IPEndPoint>()
            Combinators.Pickler.wrap (fun ep -> new TcpActorClient<'T>(ep)) (fun c -> c.ServerEndPoint) epp


    type TcpActor private () =

        static let parseEndpoint (endpoint : string) =
            let tokens = endpoint.Split(':')
            if tokens.Length <> 2 then raise <| new FormatException("invalid endpoint")
            let ipAddr = Dns.GetHostAddresses(tokens.[0]) |> Array.find (fun a -> a.AddressFamily = AddressFamily.InterNetwork)
            let port = int <| tokens.[1]
            new IPEndPoint(ipAddr, port)

        static member SetDefaultPickler(p : FsPicklerSerializer, ?overwrite) = PicklerRegistry.SetDefaultPickler(p, ?overwrite = overwrite)

        static member Create<'T>(behaviour : MailboxProcessor<'T> -> Async<unit>, ipEndPoint : IPEndPoint, ?pickler : FsPicklerSerializer) =
            let mailbox = MailboxProcessor.Start behaviour
            new TcpActor<'T>(mailbox, ipEndPoint, ?pickler = pickler)

        static member Create<'T>(behaviour : MailboxProcessor<'T> -> Async<unit>, ipEndPoint : string, ?pickler : FsPicklerSerializer) =
            TcpActor.Create(behaviour, parseEndpoint ipEndPoint, ?pickler = pickler)

        static member Connect<'T>(serverEndPoint : IPEndPoint, ?pickler : FsPicklerSerializer) =
            new TcpActorClient<'T>(serverEndPoint, ?pickler = pickler)

        static member Connect<'T>(serverEndPoint : string, ?pickler : FsPicklerSerializer) =
            new TcpActorClient<'T>(parseEndpoint serverEndPoint, ?pickler = pickler)