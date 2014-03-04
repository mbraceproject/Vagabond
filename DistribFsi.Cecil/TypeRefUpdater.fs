module Nessos.DistribFsi.TypeRefUpdater

    open System
    open System.Reflection

    open Mono.Collections.Generic
    open Mono.Cecil
    open Mono.Cecil.Cil

    module Collection =
        let update (f : 'T -> 'T) (collection : Collection<'T>) =
            let updated = collection |> Seq.map f |> Seq.toArray
            collection.Clear()
            for t in updated do collection.Add(t)


    let rec updateTypeDefinition (updateF : TypeReference -> TypeReference) (t : TypeDefinition) =

        if t.BaseType <> null then t.BaseType <- updateF t.BaseType

        Seq.iter (updateCustomAttribute updateF) t.CustomAttributes

        for gparam in t.GenericParameters do
            Collection.update updateF gparam.Constraints
            Seq.iter (updateCustomAttribute updateF) gparam.CustomAttributes

        Collection.update updateF t.Interfaces

        Seq.iter (updateFieldRef updateF) t.Fields

        Seq.iter (updatePropertyRef updateF) t.Properties

        Seq.iter (updateEventRef updateF) t.Events

        Seq.iter (updateMethodDefinition updateF) t.Methods

        Seq.iter (updateTypeDefinition updateF) t.NestedTypes

    and updateCustomAttribute (updateF : TypeReference -> TypeReference) (attr : CustomAttribute) =
        do updateMethodRef updateF attr.Constructor
            
    and updateMethodRef (updateF : TypeReference -> TypeReference) (m : MethodReference) : unit =
        m.DeclaringType <- updateF m.DeclaringType
        m.ReturnType <- updateF m.ReturnType

        for gparam in m.GenericParameters do
            Collection.update updateF gparam.Constraints
            Seq.iter (updateCustomAttribute updateF) gparam.CustomAttributes

        for param in m.Parameters do
            param.ParameterType <- updateF param.ParameterType

    and updateFieldRef (updateF : TypeReference -> TypeReference) (f : FieldReference) =
        f.DeclaringType <- updateF f.DeclaringType
        f.FieldType <- updateF f.FieldType

    and updatePropertyRef (updateF : TypeReference -> TypeReference) (p : PropertyReference) =
        p.DeclaringType <- updateF p.PropertyType
        p.PropertyType <- updateF p.PropertyType

    and updateEventRef (updateF : TypeReference -> TypeReference) (e : EventReference) =
        e.DeclaringType <- updateF e.DeclaringType
        e.EventType <- updateF e.EventType

    and updateMethodDefinition (updateF : TypeReference -> TypeReference) (m : MethodDefinition) =

        Seq.iter (updateCustomAttribute updateF) m.CustomAttributes

        for gparam in m.GenericParameters do
            Collection.update updateF gparam.Constraints
            Seq.iter (updateCustomAttribute updateF) gparam.CustomAttributes

        m.ReturnType <- updateF m.ReturnType

        for p in m.Parameters do
            p.ParameterType <- updateF p.ParameterType

        for v in m.Body.Variables do
            v.VariableType <- updateF v.VariableType

        if m.HasBody then
            for instr in m.Body.Instructions do
                updateInstruction updateF instr

    and updateInstruction (updateF : TypeReference -> TypeReference) (instr : Instruction) =
        match instr.Operand with
        | :? TypeReference as tyRef ->  instr.Operand <- updateF tyRef
        | :? ParameterDefinition as p -> p.ParameterType <- updateF p.ParameterType
        | :? VariableDefinition as v -> v.VariableType <- updateF v.VariableType
        | :? MethodReference as m -> updateMethodRef updateF m
        | :? PropertyReference as p -> updatePropertyRef updateF p
        | :? FieldReference as f -> updateFieldRef updateF f
        | :? Instruction as instr -> updateInstruction updateF instr
        | :? (Instruction []) as instructions -> Seq.iter (updateInstruction updateF) instructions
        | _ -> ()