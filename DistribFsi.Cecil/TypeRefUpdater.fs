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

    type ObjectTracker() =
        let objectCounter = new System.Runtime.Serialization.ObjectIDGenerator()

        member __.IsFirstOccurence<'T when 'T : not struct>(x : 'T) =
            if obj.ReferenceEquals(x,null) then false
            else
                let _,firstTime = objectCounter.GetId x in firstTime

    let remapTypeReferences (updateF : TypeReference -> TypeReference) (ts : seq<TypeDefinition>) =

        let tracker = new ObjectTracker()            

        let rec updateTypeReference (t : TypeReference) =
            if not <| tracker.IsFirstOccurence t then t else

            match t with
            | null -> t
            | :? GenericParameter as p ->
                match p.DeclaringMethod with
                | null -> // is type-level variable
                    let dt = updateF p.DeclaringType
                    let p' = dt.GenericParameters |> Seq.find (fun p' -> p'.Name = p.Name)
                    p' :> TypeReference
                | m -> // is method-level variable
                    do updateMethodRef m
                    p :> TypeReference

            | :? GenericInstanceType as gi ->

                let updated = updateF gi.ElementType
                let genericType = new GenericInstanceType(updated)

                for arg in gi.GenericArguments do
                    genericType.GenericArguments.Add(updateTypeReference arg)

                genericType :> TypeReference

            | t -> updateF t

        and updateTypeDefinition (t : TypeDefinition) =
            tracker.IsFirstOccurence t |> ignore

            if t.BaseType <> null then t.BaseType <- updateTypeReference t.BaseType

            Seq.iter updateCustomAttribute t.CustomAttributes

            for gparam in t.GenericParameters do
                Collection.update updateTypeReference gparam.Constraints
                Seq.iter updateCustomAttribute gparam.CustomAttributes

            Collection.update updateTypeReference t.Interfaces

            Seq.iter updateFieldRef t.Fields

            Seq.iter updatePropertyRef t.Properties

            Seq.iter updateEventRef t.Events

            Seq.iter updateMethodDefinition t.Methods

            Seq.iter updateTypeDefinition t.NestedTypes

        and updateCustomAttribute (attr : CustomAttribute) =
            do updateMethodRef attr.Constructor
            
        and updateMethodRef (m : MethodReference) =
            if not <| tracker.IsFirstOccurence m then () else

            m.DeclaringType <- updateTypeReference m.DeclaringType
            m.ReturnType <- updateTypeReference m.ReturnType

            for gparam in m.GenericParameters do
                Collection.update updateTypeReference gparam.Constraints
                Seq.iter updateCustomAttribute gparam.CustomAttributes

            for param in m.Parameters do
                param.ParameterType <- updateTypeReference param.ParameterType

        and updateFieldRef (f : FieldReference) =
            if not <| tracker.IsFirstOccurence f then () else

            f.DeclaringType <- updateTypeReference f.DeclaringType
            f.FieldType <- updateTypeReference f.FieldType

        and updatePropertyRef (p : PropertyReference) =
            if not <| tracker.IsFirstOccurence p then () else

            p.DeclaringType <- updateTypeReference p.PropertyType
            p.PropertyType <- updateTypeReference p.PropertyType

        and updateEventRef (e : EventReference) =
            if not <| tracker.IsFirstOccurence e then () else

            e.DeclaringType <- updateTypeReference e.DeclaringType
            e.EventType <- updateTypeReference e.EventType

        and updateMethodDefinition (m : MethodDefinition) =
            tracker.IsFirstOccurence m |> ignore

            Seq.iter updateCustomAttribute m.CustomAttributes

            for gparam in m.GenericParameters do
                Collection.update updateTypeReference gparam.Constraints
                Seq.iter updateCustomAttribute gparam.CustomAttributes

            m.ReturnType <- updateTypeReference m.ReturnType

            for p in m.Parameters do
                p.ParameterType <- updateTypeReference p.ParameterType

            for v in m.Body.Variables do
                v.VariableType <- updateTypeReference v.VariableType

            if m.HasBody then
                for instr in m.Body.Instructions do
                    updateInstruction instr

        and updateInstruction (instr : Instruction) =
            match instr.Operand with
            | :? TypeReference as tyRef ->  instr.Operand <- updateTypeReference tyRef
            | :? ParameterDefinition as p -> p.ParameterType <- updateTypeReference p.ParameterType
            | :? VariableDefinition as v -> v.VariableType <- updateTypeReference v.VariableType
            | :? MethodReference as m -> updateMethodRef m
            | :? PropertyReference as p -> updatePropertyRef p
            | :? FieldReference as f -> updateFieldRef f
            | :? Instruction as instr -> updateInstruction instr
            | :? (Instruction []) as instructions -> Array.iter updateInstruction instructions
            | _ -> ()


        Seq.iter updateTypeDefinition ts