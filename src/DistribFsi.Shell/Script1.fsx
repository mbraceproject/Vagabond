#r "bin/Debug/DistribFsi.Settings.dll"
#r "../../samples/ThunkServer/bin/Debug/ThunkServer.exe"
#r "bin/Debug/Mono.Cecil.dll"
#r "../../lib/Mono.Reflection.dll"


open System
open System.Reflection

open Mono.Reflection
open Mono.Cecil

let ass = Assembly.GetExecutingAssembly()


type CompiledAssemblyState =
    {
        CompiledAssemblyCount : int

        FsiDynamicAssembly : Assembly
        TypeIndex : Map<string, Assembly>
    }

module Map =
    let addMany (m : Map<'K,'V>) (kvs : ('K * 'V) seq) =
        Seq.fold (fun (m : Map<_,_>) (k,v) -> m.Add(k,v)) m kvs


let getAssemblyDiff (state : CompiledAssemblyState) =
    let snapshot = Mono.Reflection.AssemblySaver.Read(state.FsiDynamicAssembly)
    let mainModule = snapshot.MainModule
    let types = mainModule.Types
    let compiled, newTypes = 
        mainModule.Types
        |> Seq.toList
        |> List.partition (fun td -> state.TypeIndex.ContainsKey td.FullName)

    // TODO: resolve current interaction name

    for t in compiled do types.Remove t |> ignore

    let n = state.CompiledAssemblyCount + 1
    let target = sprintf "C:/mbrace/%d.dll" n

    let name = mainModule.Assembly.Name.Name <- sprintf "FSI_%03d" n

    do snapshot.Write(target)

    let assembly = Assembly.ReflectionOnlyLoadFrom(target)

    let typeIndex = newTypes |> Seq.map (fun t -> t.FullName, assembly) |> Map.addMany state.TypeIndex

    {
        CompiledAssemblyCount = n
        FsiDynamicAssembly = state.FsiDynamicAssembly
        TypeIndex = typeIndex
    }

let state = { CompiledAssemblyCount = 0 ; FsiDynamicAssembly = ass ; TypeIndex = Map.empty }
let state = getAssemblyDiff state

type Foo = Bar of int
let x = Bar 42


///

type Foo = Bar

let rec collect (tds : seq<TypeDefinition>) =
    seq {
        for t in tds do
            yield t
            yield! collect t.NestedTypes
    }

let snapshot = AssemblySaver.Read(ass)

let ts = snapshot.MainModule.Types |> collect |> Seq.toList

let t = ts.[15]

t.Fields