namespace Nessos.Vagabond.Tests

open NUnit.Framework

open Nessos.Vagabond

#nowarn "1571"

[<TestFixture>]
module ``Generic Vagabond API tests`` =

    [<Test>]
    let ``Unmanaged Assembly loading`` () =
        let path = typeof<int>.Assembly.Location
        let instance = Vagabond.Initialize()
        let info = instance.RegisterNativeDependency(path)
        info.Id.IsManaged |> shouldEqual false
        info.Image |> shouldEqual path
        info.Symbols |> shouldEqual None
        Option.isNone info.Metadata |> shouldEqual true
        let info' = instance.NativeDependencies.[0]
        info'.Id |> shouldEqual info.Id
        let instance' = Vagabond.Initialize()
        let loadInfo = instance'.LoadVagabondAssembly info
        let info'' = instance'.NativeDependencies.Head
        info''.Id |> shouldEqual info.Id