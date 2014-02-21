#r "bin/Debug/FsPickler.dll"
#r "bin/Debug/DistribFsi.Settings.dll"
#r "bin/Debug/Mono.Cecil.dll"

open System
open System.Text
open System.Reflection

open Mono.Cecil
open Mono.Cecil.Cil

open Nessos.DistribFsi

let fsiDynamicAssembly = System.Reflection.Assembly.LoadFrom("C:/Users/eirik/Desktop/test.dll")

let allFlags = BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Public

let deserializeM = typeof<Nessos.DistribFsi.SerializationSupport>.GetMethod("UnPickleOfString")

distribFsi.InstallDefaultClientSerializer()
//let x = new FsPicklerSerializer() :> ISerializer
//Nessos.DistribFsi.Settings.distribFsi.InstallClientSerializer <| x

let eraseTypeCtors (path : string) (newPath : string) =
    let defn = AssemblyDefinition.ReadAssembly path
    let mainModule = defn.MainModule
    let deserializer = mainModule.Import(deserializeM)
    let startupClasses =
        mainModule.Types 
        |> Seq.choose (fun t -> 
            if t.FullName.StartsWith("<StartupCode$") then
                match t.Methods |> Seq.tryFind(fun m -> m.Name = ".cctor") with
                | None -> None
                | Some ctor -> Some(t, ctor)
            else None)
        |> Seq.toList


    let eraseStartupClassCctor (t : TypeDefinition) (ctor : MethodDefinition) =
        let t0 = fsiDynamicAssembly.GetType(t.FullName)
        let extractAndPickleValue (f : FieldDefinition) =
            let field = t0.GetField(f.Name, allFlags)
            let value = field.GetValue(null)
            let pickle = Nessos.DistribFsi.SerializationSupport.PickleToString value
            f, pickle

        let generateUnPickleInstructions (gen : ILProcessor) (f : FieldDefinition) (pickle:string) =
            seq {
                yield gen.Create(OpCodes.Ldstr, pickle)
                yield gen.Create(OpCodes.Call, deserializer)
                if f.FieldType.IsValueType then
                    yield gen.Create(OpCodes.Unbox_Any, f.FieldType)
                else
                    yield gen.Create(OpCodes.Castclass, f.FieldType)
                yield gen.Create(OpCodes.Stsfld, f)

            } |> Seq.iter gen.Append

        // force pickling of all fields before proceeding
        let fieldData = t.Fields |> Seq.filter(fun f -> f.IsStatic) |> Seq.map extractAndPickleValue |> Seq.toList

        let ilProc = ctor.Body.GetILProcessor()

        do ilProc.Body.Instructions.Clear()

        for field, pickle in fieldData do
            generateUnPickleInstructions ilProc field pickle

        ilProc.Append(ilProc.Create(OpCodes.Ret))

    for ty, cctor in startupClasses do
        eraseStartupClassCctor ty cctor

    defn.Write(newPath)
                


eraseTypeCtors "C:/Users/eirik/Desktop/test2.dll" "C:/Users/eirik/Desktop/test2.dll"



#r "C:/Users/eirik/Desktop/test.dll"

FSI_0002.f 1821

let string = Nessos.DistribFsi.SerializationSupport.PickleToString 12

Nessos.DistribFsi.SerializationSupport.UnPickleOfString string