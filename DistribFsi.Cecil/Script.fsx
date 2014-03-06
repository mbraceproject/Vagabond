#r "bin/Debug/FsPickler.dll"
#r "bin/Debug/Mono.Cecil.dll"
#r "bin/Debug/Mono.Reflection.dll"
#r "bin/Debug/DistribFsi.Cecil.dll"
//#r "../packages/Mono.Cecil.0.9.5.4/lib/net40/Mono.Cecil.Rocks.dll"

open Mono.Cecil
open Mono.Reflection

open Nessos.DistribFsi
open Nessos.DistribFsi.FsiAssemblyCompiler

touch ()

let state = DynamicAssemblyInfo.Init <| System.Reflection.Assembly.GetExecutingAssembly()

let state = compileDynamicAssemblySlice state

type Foo<'T> (x : 'T) =
    member __.Value = x

    static member Create<'S>(x : 'S) = Foo<'S>(x)

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

Mono.Reflection.AssemblySaver.SaveTo(System.Reflection.Assembly.GetExecutingAssembly(), "/mbrace/3.dll")

#r "C:/mbrace/FSI_001.dll"
#r "C:/mbrace/FSI_002.dll"
#r "C:/mbrace/FSI_003.dll"
#r "C:/mbrace/FSI_004.dll"
//#r "C:/mbrace/3.dll"

FSI_0003.Foo<_>((12,"hello"))
FSI_0005.Bar<_>((12,"hello"), "hello")
FSI_0005.foo
let it = FSI_0009.Bar<_>([|12|], "hello")
//FSI_0005.Bar<_>("hello", "hello")
//
//typeof<FSI_0005.Bar<int * string>>