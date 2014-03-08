#r "bin/Debug/FsPickler.dll"
#r "bin/Debug/Mono.Cecil.dll"
#r "bin/Debug/Mono.Reflection.dll"
#r "bin/Debug/DistribFsi.Cecil.dll"
//#r "../packages/Mono.Cecil.0.9.5.4/lib/net40/Mono.Cecil.Rocks.dll"

open Mono.Cecil
open Mono.Reflection

open Nessos.DistribFsi

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

let b = FSI_0002.Foo.NewBar 42

FSI_0004.incr b

let ass = AssemblyDefinition.ReadAssembly("C:/Users/eirik/Desktop/a.dll")

let t = ass.MainModule.Types |> Seq.nth 3
let flds = t.Fields |> Seq.toList |> Seq.head

let m = t.Methods |> Seq.head
let foo = m.Body.Instructions |> Seq.nth 1

let fr = foo.Operand :?> FieldReference