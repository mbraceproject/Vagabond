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

let state = CompiledAssemblyState.Empty

let state = compilePendingInteractions state

//stack overflow
type Foo<'T> (x : 'T) =
    member __.Value = x
    
type Bar<'T>(x : 'T, y : string) =
    inherit Foo<'T>(x)
    member __.Value2 = y

Mono.Reflection.AssemblySaver.SaveTo(System.Reflection.Assembly.GetExecutingAssembly(), "/mbrace/3.dll")

#r "C:/mbrace/1.dll"
#r "C:/mbrace/2.dll"

FSI_0003.Foo 42

let f () =
    let bar = FSI_0005.Bar(42, "forty-two")
    bar.GetHashCode()