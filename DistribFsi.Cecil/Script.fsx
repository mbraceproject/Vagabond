#r "bin/Debug/FsPickler.dll"
#r "bin/Debug/Mono.Cecil.dll"
#r "bin/Debug/Mono.Reflection.dll"
#r "bin/Debug/DistribFsi.Cecil.dll"
//#r "../packages/Mono.Cecil.0.9.5.4/lib/net40/Mono.Cecil.Rocks.dll"

open Mono.Cecil
open Mono.Reflection

open Nessos.DistribFsi
open Nessos.DistribFsi.FsiAssemblyCompiler

type Foo<'T> (x : 'T) =
    member __.Value = x

    static member Create<'S>(x : 'S) = Foo<'S>(x)

let deps = getObjectDependencies <@ abs @>

let foo = Foo<int * string>(42, "forty-two")
    
type Bar<'T>(x : 'T, y : string) =
    inherit Foo<'T>(x)

    let copy = Foo<int>.Create<'T>(x)

    member __.Value2 = y
    member __.Copy = copy
    member __.GetLam() = fun x -> copy.GetHashCode() + x
//
//type Foo(x : int) =
//    member __.Value = x
//    
//type Bar(x : int, y : string) =
//    inherit Foo(x)
//    member __.Value2 = y

[<ReflectedDefinition>]
let abs x = if x > 0 then x else -x

Mono.Reflection.AssemblySaver.SaveTo(System.Reflection.Assembly.GetExecutingAssembly(), "/mbrace/3.dll")

#r "/mbrace/FSI-ASSEMBLY_9829c6ae-19b9-4a7c-974b-09fcb8b9c8c6_001.dll"

let (Quotations.Patterns.Call(_,m,_)) = <@ FSI_0003.abs 2 @>

Quotations.Expr.TryGetReflectedDefinition m