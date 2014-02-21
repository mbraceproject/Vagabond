module internal Nessos.DistribFsi.Shell.Cecil

    open System
    open System.Text
    open System.Reflection

    open Mono.Cecil
    open Mono.Cecil.Cil

    let fsiDynamicAssembly = System.Reflection.Assembly.LoadFrom("/Users/eirik/Desktop/test.dll")

    let allFlags = BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Public

    let deserializeM = typeof<Nessos.DistribFsi.SerializationSupport>.GetMethod("UnPickleOfString")

    let generateUnPickler (mainModule : ModuleDefinition) (fieldType : Type) (f : FieldDefinition) (pickle : string) =
        let deserializer = deserializeM.MakeGenericMethod [| fieldType |]
        let deserializer = mainModule.Import deserializer
        [
            yield Instruction.Create(OpCodes.Ldstr, pickle)
            yield Instruction.Create(OpCodes.Call, deserializer)
            yield Instruction.Create(OpCodes.Stfld, f)
        ] 

    let gatherStartupClasses (path : string) =
        let defn = AssemblyDefinition.ReadAssembly path
        let mainModule = defn.MainModule
        mainModule.Types 
        |> Seq.choose (fun t -> 
            if t.FullName.StartsWith("<StartupCode$") then
                match t.Methods |> Seq.tryFind(fun m -> m.Name = ".cctor") with
                | None -> None
                | Some ctor ->
                    let t0 = fsiDynamicAssembly.GetType(t.FullName)
                    let values = 
                        t.Fields 
                        |> Seq.filter (fun f -> f.IsStatic)
                        |> Seq.map (fun f -> 
                            let value = t0.GetField(f.Name, allFlags).GetValue(null)
//                            let bytes = try Nessos.DistribFsi.SerializationSupport.Pickle value |> Some with e -> None
                            let bytes = Some [| 1uy |]
                            f, bytes
                            ) |> Seq.toArray
                    Some(values, t, ctor)
            else
                None)
        |> Seq.toArray



    let ctype = gatherStartupClasses "/Users/eirik/Desktop/test.dll"

    let first =
        let m = ctype().[0]
        let ilProc = m.Body.GetILProcessor()
        for instr in ilProc.Body.Instructions do
            printfn "%A" instr