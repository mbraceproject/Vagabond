namespace Nessos.Vagabond.Tests

open System
open System.Reflection
open System.Web

open NUnit.Framework

open Nessos.Vagabond

#nowarn "1571"

[<TestFixture>]
module ``Generic Vagabond API tests`` =
    
    let t = typeof<System.Web.BeginEventHandler>

    [<Test>]
    let ``Unmanaged Assembly loading`` () =
        let path = typeof<int>.Assembly.Location
        let instance = Vagabond.Initialize()
        let info = instance.RegisterNativeDependency(path)
        info.Metadata.IsManagedAssembly |> shouldEqual false
        info.Image |> shouldEqual path
        info.Symbols |> shouldEqual None
        info.Metadata.IsDynamicAssemblySlice |> shouldEqual false
        let info' = instance.NativeDependencies.[0]
        info'.Id |> shouldEqual info.Id
        let instance' = Vagabond.Initialize()
        let loadInfo = instance'.LoadVagabondAssembly info
        let info'' = instance'.NativeDependencies.[0]
        info''.Id |> shouldEqual info.Id

    [<Test>]
    let ``Cyclic Assembly resolution`` () =
        let load (name:string) =
            try [| Assembly.ReflectionOnlyLoad(name) |]
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