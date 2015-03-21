namespace Nessos.Vagabond.Tests

open NUnit.Framework

open Nessos.Vagabond


[<TestFixture>]
module ``Generic Vagabond API tests`` =

    [<Test>]
    let ``Unmanaged Assembly loading`` () =
        let path = typeof<int>.Assembly.Location
        let instance = Vagabond.Initialize()
        let info = instance.IncludeUnmanagedAssembly(path)
        info.Id.IsManaged |> shouldEqual false
        info.Image |> shouldEqual path
        info.Symbols |> shouldEqual None
        Option.isNone info.Metadata |> shouldEqual true
        let info' = instance.UnManagedDependencies.[0]
        info'.Id |> shouldEqual info.Id
        let instance' = Vagabond.Initialize()
        let loadInfo = instance'.LoadVagabondAssembly info
        let info'' = instance'.UnManagedDependencies.Head
        info''.Id |> shouldEqual info.Id