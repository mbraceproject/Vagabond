namespace Nessos.Vagrant.Tests

open System
open System.Threading
open System.IO

open Nessos.Thespian
open Nessos.Thespian.Serialization
open Nessos.Thespian.Remote
open Nessos.Thespian.Remote.TcpProtocol

open Nessos.Vagrant

/// Vagrant configuration container
type VagrantConfig private () =

    // vagrant initialization
    static let ignoredAssemblies =
        let this = System.Reflection.Assembly.GetExecutingAssembly()
        let dependencies = Utilities.ComputeAssemblyDependencies(this, requireLoadedInAppDomain = false)
        new System.Collections.Generic.HashSet<_>(dependencies)

    static let vagrant =
        let cacheDir = Path.Combine(Path.GetTempPath(), sprintf "thunkServerCache-%O" <| Guid.NewGuid())
        let _ = Directory.CreateDirectory cacheDir
        Vagrant.Initialize(cacheDirectory = cacheDir, isIgnoredAssembly = ignoredAssemblies.Contains)

    static member Vagrant = vagrant
    static member Pickler = vagrant.Pickler

/// Actor configuration tools
type Actor private () =

    static do
        let _ = System.Threading.ThreadPool.SetMinThreads(100, 100) 
        defaultSerializer <- new FsPicklerMessageSerializer(VagrantConfig.Pickler)
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