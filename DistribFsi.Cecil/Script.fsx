#r "bin/Debug/FsPickler.dll"
#r "bin/Debug/Mono.Cecil.dll"
#r "bin/Debug/Mono.Reflection.dll"
#r "bin/Debug/DistribFsi.Cecil.dll"
//#r "../packages/Mono.Cecil.0.9.5.4/lib/net40/Mono.Cecil.Rocks.dll"

open Mono.Cecil
open Mono.Reflection

open Nessos.DistribFsi
open Nessos.DistribFsi.FsiAssemblyCompiler

SerializationSupport.RegisterSerializer <| FsPicklerSerializer()

let state = CompiledAssemblyState.Empty

let state = compilePendingInteractions state


type Foo (x : int) =
    member __.Value = x
    
type Bar(x : int, y : string) =
    inherit Foo(x)
    member __.Value2 = y

Mono.Reflection.AssemblySaver.SaveTo(System.Reflection.Assembly.GetExecutingAssembly(), "/mbrace/3.dll")

#r "/mbrace/1.dll"
#r "/mbrace/2.dll"


let bar = FSI_0005.Bar(42, "forty-two")