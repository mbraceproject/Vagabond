#r "bin/Debug/FsPickler.dll"
#r "bin/Debug/Mono.Cecil.dll"
#r "bin/Debug/Mono.Reflection.dll"
#r "bin/Debug/Vagrant.Cecil.dll"
//#r "../packages/Mono.Cecil.0.9.5.4/lib/net40/Mono.Cecil.Rocks.dll"

open Mono.Cecil
open Mono.Reflection

open Nessos.Vagrant

let srv = CompilationServer.Create(path = "C:/mbrace/")

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


let deps = srv.ComputePortableDependencies <@ foo @>



#I "C:/Users/eirik/Desktop"
#r "a.dll"
#r "b.dll"

#time
let ass= System.Reflection.Assembly.GetExecutingAssembly()
let foo = AssemblySaver.Read(ass)