module Nessos.DistribFsi.TypeRefUpdater

    open System
    open System.Reflection

    open Mono.Cecil
    open Mono.Cecil.Cil

    type IReferenceUpdater =
        abstract UpdateTypeRef : TypeReference -> TypeReference option
//        abstract UpdateMethodRef : MethodReference -> MethodReference option
//        abstract UpdatePropertyRef : PropertyReference -> PropertyReference option
//        abstract UpdateFieldRef : FieldReference -> FieldReference option

    let updateTypeRef (updater : IReferenceUpdater) (t : TypeReference) =
        defaultArg (updater.UpdateTypeRef t) t

    let rec updateMethodRef (updater : IReferenceUpdater) (m : MethodReference) =
        match updater.UpdateTypeRef m.DeclaringType with
        | None -> None
        | Some dt ->
            let m1 = new MethodReference(m.Name, updateTypeRef updater m.ReturnType, dt)
            let m1 =
                match m with
                | :? GenericInstanceMethod as m ->
                    let m1 = new GenericInstanceMethod(m1)
                    for ga in m.GenericArguments |> Seq.map (updateTypeRef updater) do
                          m1.GenericArguments.Add(ga)
                    m1 :> MethodReference
                | _ -> m1

            Some m1

    and updateFieldRef (updater : IReferenceUpdater) (f : FieldReference) =
        match updater.UpdateTypeRef f.DeclaringType with
        | None -> None
        | Some dt -> Some <| new FieldReference(f.Name, updateTypeRef updater f.FieldType, dt)

    and updatePropertyRef (updater : IReferenceUpdater) (p : PropertyReference) =
        // Cecil legends say that PropertyDefinition is the only implementation of PropertyReference
        let p = p :?> PropertyDefinition in
        match updater.UpdateTypeRef p.DeclaringType with
        | None -> None
        | Some dt -> 
            let p' = new PropertyDefinition(p.Name, p.Attributes, updateTypeRef updater p.PropertyType)
            Some (p' :> PropertyReference)

    and updateEventRef (updater : IReferenceUpdater) (e : EventReference) =
        let e = e :?> EventDefinition
        match updater.UpdateTypeRef e.DeclaringType with
        | None -> None
        | Some dt -> 
            let e' = new EventDefinition(e.Name, e.Attributes, updateTypeRef updater e.EventType)
            Some (e' :> EventReference)

    and updateTypeDefinition (updater : IReferenceUpdater) (t : TypeDefinition) =

        updater.UpdateTypeRef t.BaseType |> Option.iter (fun t' -> t.BaseType <- t')

        for i in t.Interfaces do
            updater.UpdateTypeRef i |> Option.iter (fun i' -> t.Interfaces.Remove(i) |> ignore ; t.Interfaces.Add(i'))

        for f in t.Fields do
            updater.UpdateTypeRef f.FieldType |> Option.iter (fun t -> f.FieldType <- t)

        for p in t.Properties do    
            updater.UpdateTypeRef p.PropertyType |> Option.iter (fun t -> p.PropertyType <- t)

        for e in t.Events do
            updater.UpdateTypeRef e.EventType |> Option.iter (fun t -> e.EventType <- t)

        for nt in t.NestedTypes do
            updateTypeDefinition updater nt

        for m in t.Methods do
            updateMethodDefinition updater m


    and updateMethodDefinition (updater : IReferenceUpdater) (m : MethodDefinition) =
    
        updater.UpdateTypeRef m.ReturnType |> Option.iter (fun t -> m.ReturnType <- t)

        for p in m.Parameters do
            updater.UpdateTypeRef p.ParameterType |> Option.iter (fun t -> p.ParameterType <- t)

        for v in m.Body.Variables do
            updater.UpdateTypeRef v.VariableType |> Option.iter (fun t -> v.VariableType <- t)

        if m.HasBody then
    //        let ilProc = m.Body.GetILProcessor()
    //
    //        let instructions = ilProc.Body.Instructions |> Seq.toArray
    //        ilProc.Body.Instructions.Clear()

            for instr in m.Body.Instructions do
                updateInstruction updater instr

    and updateInstruction (updater : IReferenceUpdater) (instr : Instruction) =
        match instr.Operand with
        | :? TypeReference as tyRef -> 
            updater.UpdateTypeRef tyRef |> Option.iter (fun t -> instr.Operand <- t)
        | :? ParameterDefinition as p ->
            updater.UpdateTypeRef p.ParameterType |> Option.iter (fun t -> p.ParameterType <- t)
        | :? VariableDefinition as v ->
            updater.UpdateTypeRef v.VariableType |> Option.iter (fun t -> v.VariableType <- t)
        | :? MethodReference as m ->
            updateMethodRef updater m |> Option.iter (fun m -> instr.Operand <- m)
        | :? PropertyReference as p ->
            updatePropertyRef updater p |> Option.iter (fun p -> instr.Operand <- p)
        | :? FieldReference as f ->
            updateFieldRef updater f |> Option.iter (fun f -> instr.Operand <- f)
        | :? Instruction as instr ->
            updateInstruction updater instr
        | :? (Instruction []) as instructions ->
            for instr in instructions do
                updateInstruction updater instr
        | _ -> ()

