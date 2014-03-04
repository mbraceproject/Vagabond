#r "bin/Debug/Mono.Cecil.dll"
#r "bin/Debug/Mono.Reflection.dll"
#r "bin/Debug/DistribFsi.Cecil.dll"
//#r "../packages/Mono.Cecil.0.9.5.4/lib/net40/Mono.Cecil.Rocks.dll"

open Mono.Cecil
open Mono.Reflection

open Nessos.DistribFsi.FsiAssemblyCompiler

let state = CompiledAssemblyState.Empty

let state = compilePendingInteractions state


type Foo = Foo of int
let mutable y = Foo(12)

#r "/mbrace/1.dll"
#r "/mbrace/2.dll"