namespace MBrace.Vagabond.Tests

open System
open System.Reflection

open Xunit

open MBrace.Vagabond

#nowarn "1571"

module ``Generic Vagabond API tests`` =

    [<Fact>]
    let ``Unmanaged Assembly loading`` () =
        let path = typeof<int>.Assembly.Location
        let instance = Vagabond.Initialize()
        let info = instance.RegisterNativeDependency(path)
        info.Metadata.IsNativeAssembly |> shouldEqual true
        info.Image |> shouldEqual path
        info.Symbols |> shouldEqual None
        info.Metadata.IsDynamicAssemblySlice |> shouldEqual false
        let info' = instance.NativeDependencies.[0]
        info'.Id |> shouldEqual info.Id
        let instance' = Vagabond.Initialize()
        let loadInfo = instance'.LoadVagabondAssembly info
        let info'' = instance'.NativeDependencies.[0]
        info''.Id |> shouldEqual info.Id

    [<Fact>]
    let ``Cyclic Assembly resolution`` () =
        let load (name:string) =
            try [| Assembly.Load(name) |]
            with _ -> [||]

        let cyclicAssemblies =
            [|
                yield! load "System.Web, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
                yield! load "System.Web.Services, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
                yield! load "System.Printing, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35"
                yield! load "PresentationFramework, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35"
            |]

        let lookup (o:obj) = 
            Vagabond.ComputeAssemblyDependencies(o, policy = AssemblyLookupPolicy.None)
            |> ignore
        
        lookup cyclicAssemblies
        for a in cyclicAssemblies do lookup a