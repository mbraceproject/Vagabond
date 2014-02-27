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
let mutable y = Foo(12)
let f () = y.Value


//
let ass = AssemblySaver.Read state.FsiDynamicAssembly
ass.MainModule.Name <- "poutsa.dll"
ass.Name.Name <- "poutsa"
ass.Name.Version <- new System.Version(2,0)

ass.Write("/mbrace/bar.dll")
let modl = ass.MainModule
let t = modl.Types |> Seq.toList |> Seq.nth 5
let nt = t.NestedTypes |> Seq.nth 0

let tp =  nt.GenericParameters |> Seq.head


tp.DeclaringType

//  TODO : typarams

#r "/mbrace/1.dll"
#r "/mbrace/2.dll"


let f = new FSI_0003.Foo<_>(("poutses", 42))

FSI_0006.f()

let ass = System.AppDomain.CurrentDomain.GetAssemblies() |> Seq.find(fun a -> a.GetName().Name = "FSI_002")
let t = ass.GetTypes() |> Array.find (fun t -> t.FullName =  "<StartupCode$FSI_0005>.$FSI_0005" )
let m = t.GetMethods() |> Seq.head

m.Invoke(null, [||])

open System.Reflection

let asses = System.AppDomain.CurrentDomain.GetAssemblies()

let ass = System.Reflection.Assembly.LoadFile("C:/mbrace/foo1.dll")
let ass' = System.Reflection.Assembly.LoadFile("C:/mbrace/bar.dll")
ass'.Location

let t = ass.GetTypes() |> Seq.find (fun t -> t.FullName.Contains "Foo")
let t' = ass'.GetTypes() |> Seq.find (fun t -> t.FullName.Contains "Foo")


type Foo () =
    static do printfn "ha!"
    static let x = 42

    member __.Value = x

let s = typeof<Foo>.GetFields(BindingFlags.NonPublic ||| BindingFlags.Static)

let x = s.[0]
let init = s.[1]

x.GetValue(null)
init.GetValue(null)

let x = Foo ()

#r "/Users/eirik/Desktop/File1.dll"

open System.Reflection

let t = typeof<Foo.Foo>

let flds = t.GetFields(BindingFlags.NonPublic ||| BindingFlags.Static)

let x = flds.[0]
let init = flds.[1]

x.GetValue(null)
init.GetValue(null)