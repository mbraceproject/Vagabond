namespace Nessos.Vagabond.Tests

open System
open System.Reflection

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

    [<Test>]
    let ``Dot not differentiate between reflection-only and loaded assemblies`` () =
        let a1 = Assembly.Load "System.Configuration, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
        let a2 = Assembly.ReflectionOnlyLoad "System.Configuration, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
        
        a2 |> shouldNotEqual a1
        Vagabond.ComputeAssemblyDependencies([|a1 ; a2|], policy = AssemblyLookupPolicy.None) |> shouldEqual [|a1|]
        

    [<Test>]
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