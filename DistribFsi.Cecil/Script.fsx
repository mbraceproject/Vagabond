#r "bin/Debug/Mono.Cecil.dll"
#r "bin/Debug/Mono.Reflection.dll"
#r "bin/Debug/DistribFsi.Cecil.dll"
#r "../packages/Mono.Cecil.0.9.5.4/lib/net40/Mono.Cecil.Rocks.dll"

open Mono.Cecil
open Mono.Reflection

open Nessos.DistribFsi.FsiAssemblyCompiler

let state = CompiledAssemblyState.Empty

let state = compilePendingInteractions state


type Foo<'T> (x : 'T) = member __.Value = x
let y = Foo(12)

//  TODO : typarams

let ass = AssemblyDefinition.ReadAssembly("/mbrace/foo.dll")
let fooM = ass.MainModule.Types |> Seq.nth 1
let bar = fooM.NestedTypes |> Seq.nth 1

let tp = foo.GenericParameters |> Seq.head

tp.DeclaringType