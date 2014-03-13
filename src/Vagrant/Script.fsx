#I "bin/Debug/"
#r "Vagrant.dll"

open Nessos.Vagrant

let srv = new VagrantServer()

let dyn = System.Reflection.Assembly.GetExecutingAssembly()

srv.CompileDynamicAssemblySlice dyn

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