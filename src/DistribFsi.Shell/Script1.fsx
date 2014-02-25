#r "bin/Debug/DistribFsi.Settings.dll"
#r "../../samples/ThunkServer/bin/Debug/ThunkServer.exe"
#r "bin/Debug/Mono.Cecil.dll"
#r "../../lib/Mono.Reflection.dll"
//#r "../../packages/Mono.Cecil.0.9.5.4/lib/net40/Mono.Cecil.Rocks.dll"


open System
open System.Text.RegularExpressions
open System.Reflection

open Mono.Reflection
open Mono.Cecil
open Mono.Cecil.Cil

let ass = Assembly.GetExecutingAssembly()
let tryFindFsiName =
    let fsiRegex = Regex("FSI_[0-9]{4}")
    fun (name : string) ->
        let m = fsiRegex.Match(name)
        if m.Success then
            let name = m.Groups.[0].Value
            Some name
        else
            None

type CompiledAssemblyState =
    {
        CompiledAssemblyCount : int

        FsiDynamicAssembly : Assembly
        TypeIndex : Map<string, Assembly>
    }

module Map =
    let addMany (m : Map<'K,'V>) (kvs : ('K * 'V) seq) =
        Seq.fold (fun (m : Map<_,_>) (k,v) -> m.Add(k,v)) m kvs

//

let rec collect (types : seq<TypeDefinition>) =
    seq {
        for t in types do
            yield t
            yield! collect t.NestedTypes
    } |> Seq.distinct

///

let rec updateTypeDefinition (updateF : TypeReference -> TypeReference option) (t : TypeDefinition) =

    updateF t.BaseType |> Option.iter (fun t' -> t.BaseType <- t')

    for i in t.Interfaces do
        updateF i |> Option.iter (fun i' -> t.Interfaces.Remove(i) |> ignore ; t.Interfaces.Add(i'))

    for f in t.Fields do
        updateF f.FieldType |> Option.iter (fun t -> f.FieldType <- t)

    for p in t.Properties do    
        updateF p.PropertyType |> Option.iter (fun t -> p.PropertyType <- t)

    for e in t.Events do
        updateF e.EventType |> Option.iter (fun t -> e.EventType <- t)

    for nt in t.NestedTypes do
        updateTypeDefinition updateF nt

    for m in t.Methods do
        updateMethodDefinition updateF m


and updateMethodDefinition (updateF : TypeReference -> TypeReference option) (m : MethodDefinition) =
    
    updateF m.ReturnType |> Option.iter (fun t -> m.ReturnType <- t)

    for p in m.Parameters do
        updateF p.ParameterType |> Option.iter (fun t -> p.ParameterType <- t)

    for v in m.Body.Variables do
        updateF v.VariableType |> Option.iter (fun t -> v.VariableType <- t)

    if m.HasBody then
        let ilProc = m.Body.GetILProcessor()

        let instructions = ilProc.Body.Instructions |> Seq.toArray
        ilProc.Body.Instructions.Clear()

        for instr in instructions do
            updateInstruction updateF ilProc instr

and updateInstruction (updateF : TypeReference -> TypeReference option) (ilProc : ILProcessor) (instr : Instruction) =
    match instr.Operand with
    | :? TypeReference as tyRef -> 
        updateF tyRef |> Option.iter (fun t -> instr.Operand <- t)
    | :? ParameterDefinition as p ->
        updateF p.ParameterType |> Option.iter (fun t -> p.ParameterType <- t)
    | :? VariableDefinition as v ->
        updateF v.VariableType |> Option.iter (fun t -> v.VariableType <- t)
    | :? MethodReference as m ->
        updateF m.DeclaringType 
        |> Option.map (fun t -> t.Resolve().Methods |> Seq.find (fun m' -> m.ToString() = m'.ToString()))
        |> Option.iter (fun m -> instr.Operand <- m)

    | :? FieldReference as f ->
        updateF f.DeclaringType |> Option.iter (fun t -> f.DeclaringType <- t)
    | :? Instruction as instr ->
        updateInstruction updateF ilProc instr
    | :? (Instruction []) as instructions ->
        for instr in instructions do
            updateInstruction updateF ilProc instr
    | _ -> ()


let tryResolveTypeImport (main : ModuleDefinition) (state : CompiledAssemblyState) (t : TypeReference) =
    if t = null then None
    elif t.Scope.Name = main.Name then
        match tryFindFsiName t.FullName |> Option.bind state.TypeIndex.TryFind with
        | None -> None
        | Some a ->
            let rec loadType (t : TypeReference) =
                match t.DeclaringType with
                | null ->
                    let qname = t.FullName.Replace('/','.')
                    a.GetType(qname)
                | dt ->
                    let dt0 = loadType dt
                    dt0.GetNestedType(t.Name)

            let t0 = loadType t
            let t00 = main.Import t0
            Some t00
    else
        None
                    
let getAssemblyDiff (state : CompiledAssemblyState) =
    let snapshot = Mono.Reflection.AssemblySaver.Read(state.FsiDynamicAssembly)
    let mainModule = snapshot.MainModule
    let types = mainModule.Types
    let compiled, newTypes = 
        types
        |> Seq.toList
        |> List.partition (fun td -> state.TypeIndex.ContainsKey td.FullName)

    // remove already compiled types
    for t in compiled do types.Remove t |> ignore

    let compiledInteractions, currentInteraction = 
        let topLevelModuleNames = types |> Seq.choose (fun t -> tryFindFsiName t.Name) |> Set.ofSeq
        let currentInteraction = topLevelModuleNames |> Seq.max
        let compiledInteractions = topLevelModuleNames |> Set.remove currentInteraction |> Set.toList
        compiledInteractions, currentInteraction

    for t in newTypes do
        if t.FullName.Contains currentInteraction then types.Remove t |> ignore

    // remap type refs

    for t in collect types do
        updateTypeDefinition (tryResolveTypeImport mainModule state) t

    // compile

    let n = state.CompiledAssemblyCount + 1
    let target = sprintf "C:/mbrace/%d.dll" n

    let name = mainModule.Assembly.Name.Name <- sprintf "FSI_%03d" n

    do snapshot.Write(target)

    let assembly = Assembly.ReflectionOnlyLoadFrom(target)

    let typeIndex = compiledInteractions |> Seq.map (fun name -> name, assembly) |> Map.addMany state.TypeIndex

    {
        CompiledAssemblyCount = n
        FsiDynamicAssembly = state.FsiDynamicAssembly
        TypeIndex = typeIndex
    }

let state = { CompiledAssemblyCount = 0 ; FsiDynamicAssembly = ass ; TypeIndex = Map.empty }
let state = getAssemblyDiff state

type Foo = Bar

let foo = AssemblyDefinition.ReadAssembly("/mbrace/1.dll")

foo.MainModule.Name

let t = foo.MainModule.Types |> Seq.toList |> Seq.nth 5

t.Scope