namespace Nessos.Vagabond.Tests

open System
open System.Reflection
open System.Threading
open System.IO

open Nessos.Thespian
open Nessos.Thespian.Serialization
open Nessos.Thespian.Remote
open Nessos.Thespian.Remote.TcpProtocol

open Nessos.Vagabond

/// Vagabond configuration container
type VagabondConfig private () =

    static let vagrant =
        let cacheDir = Path.Combine(Path.GetTempPath(), sprintf "thunkServerCache-%O" <| Guid.NewGuid())
        let _ = Directory.CreateDirectory cacheDir
        Vagabond.Initialize(cacheDirectory = cacheDir, ignoredAssemblies = [Assembly.GetExecutingAssembly()])

    static member Vagabond = vagrant
    static member Pickler = vagrant.Pickler

/// Actor configuration tools
type Actor private () =

    static do
        let _ = System.Threading.ThreadPool.SetMinThreads(100, 100) 
        defaultSerializer <- new FsPicklerMessageSerializer(VagabondConfig.Pickler)
        Nessos.Thespian.Default.ReplyReceiveTimeout <- Timeout.Infinite
        TcpListenerPool.RegisterListener(IPEndPoint.any)

    /// Publishes an actor instance to the default TCP protocol
    static member Publish(actor : Actor<'T>, ?name) =
        let name = match name with Some n -> n | None -> Guid.NewGuid().ToString()
        actor
        |> Actor.rename name
        |> Actor.publish [ Protocols.utcp() ]
        |> Actor.start

    /// Publishes an actor instance to the default TCP protocol
    static member Publish(receiver : Receiver<'T>, ?name) =
        let name = match name with Some n -> n | None -> Guid.NewGuid().ToString()
        receiver
        |> Receiver.rename name
        |> Receiver.publish [ Protocols.utcp() ]
        |> Receiver.start

    static member EndPoint = TcpListenerPool.GetListener().LocalEndPoint