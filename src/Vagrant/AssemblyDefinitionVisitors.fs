module internal Nessos.Vagrant.AssemblyDefinitionVisitors

    open System
    open System.Collections.Generic
    open System.Reflection

    open Mono.Collections.Generic
    open Mono.Cecil
    open Mono.Cecil.Cil

    open Nessos.Vagrant.Utils

    //
    //  The following implements a visitor of sorts for Cecil TypeDefinitions. 
    //  It performs the following operations:
    //
    //    1. All type references are updated using the updateF function
    //    2. Makes all member definitions public if enabled
    //

    let updateTypeReferences makePublicMembers (updateF : TypeReference -> TypeReference option) (types : seq<TypeDefinition>) : unit =

        // keeps track of all traversed objects in the Assembly definition.
        let tracker = new ObjectTracker()
        let types = new HashSet<_>(types)  

        // the updateF function is expected to only handle named types
        // this adds support for things like type parameters, arrays and generic instances
        let rec updateTypeReference (t : TypeReference) =
            match t with
            // if is type definition within the current processing set
            // leave to the updateMethodDefinition function
            | :? TypeDefinition as td when types.Contains td -> t
            // do not handle the same instance twice
            | t when not <| tracker.IsFirstOccurence t -> t
            | :? GenericParameter as p ->
                match p.DeclaringMethod with
                | null ->
                    let dt : TypeReference = updateTypeReference p.DeclaringType
                    let p' = dt.GenericParameters |> Seq.find (fun p' -> p'.Name = p.Name)
                    p' :> TypeReference
                | m -> updateMethodReference m ; p :> TypeReference

            | :? GenericInstanceType as gi ->
                // need to define a new generic instance for this
                let updated = updateTypeReference gi.ElementType
                let genericType = new GenericInstanceType(updated)

                for arg in gi.GenericArguments do
                    genericType.GenericArguments.Add(updateTypeReference arg)

                genericType :> TypeReference

            | :? ArrayType as at ->
                let updated = updateTypeReference at.ElementType
                new ArrayType(updated) :> TypeReference

            | :? PointerType as at ->
                let updated = updateTypeReference at.ElementType
                new PointerType(updated) :> TypeReference

            | :? ByReferenceType as at ->
                let updated = updateTypeReference at.ElementType
                new ByReferenceType(updated) :> TypeReference

            | :? TypeSpecification as ts -> updateTypeReference ts.ElementType

            | t -> defaultArg (updateF t) t

        and updateMethodReference (m : MethodReference) =
            if not <| tracker.IsFirstOccurence m then () else

            match m with
            | :? GenericInstanceMethod as gm ->
                updateMethodReference gm.ElementMethod
                Collection.update updateTypeReference gm.GenericArguments

            | :? MethodSpecification as m -> updateMethodReference m.ElementMethod
            | _ ->

                m.DeclaringType <- updateTypeReference m.DeclaringType
                m.ReturnType <- updateTypeReference m.ReturnType

                for gparam in m.GenericParameters do
                    Collection.update updateTypeReference gparam.Constraints
                    Seq.iter updateCustomAttribute gparam.CustomAttributes

                for param in m.Parameters do
                    Seq.iter updateCustomAttribute param.CustomAttributes
                    param.ParameterType <- updateTypeReference param.ParameterType
                    match param.Method with
                    | :? MethodReference as m -> updateMethodReference m
                    | _ -> ()

        and updateCustomAttribute (attr : CustomAttribute) =
            do updateMethodReference attr.Constructor

        and updateFieldReference (f : FieldReference) =
            if not <| tracker.IsFirstOccurence f then () else

            if makePublicMembers then
                match f with :? FieldDefinition as f -> f.IsPublic <- true | _ -> ()

            f.DeclaringType <- updateTypeReference f.DeclaringType
            f.FieldType <- updateTypeReference f.FieldType

        and updatePropertyReference (p : PropertyReference) =
            if not <| tracker.IsFirstOccurence p then () else

            p.DeclaringType <- updateTypeReference p.PropertyType
            p.PropertyType <- updateTypeReference p.PropertyType

        and updateEventReference (e : EventReference) =
            if not <| tracker.IsFirstOccurence e then () else

            e.DeclaringType <- updateTypeReference e.DeclaringType
            e.EventType <- updateTypeReference e.EventType

        and updateMethodDefinition (m : MethodDefinition) =
            tracker.IsFirstOccurence m |> ignore

            if makePublicMembers then m.IsPublic <- true

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
            | :? MethodReference as m -> updateMethodReference m
            | :? PropertyReference as p -> updatePropertyReference p
            | :? FieldReference as f -> updateFieldReference f
            | :? Instruction as instr -> updateInstruction instr
            | :? (Instruction []) as instructions -> Array.iter updateInstruction instructions
            | _ -> ()

        and updateTypeDefinition (t : TypeDefinition) =
            tracker.IsFirstOccurence t |> ignore

            if makePublicMembers then
                if t.IsNested then t.IsNestedPublic <- true
                else t.IsPublic <- true

            if t.BaseType <> null then t.BaseType <- updateTypeReference t.BaseType

            Seq.iter updateCustomAttribute t.CustomAttributes

            for gparam in t.GenericParameters do
                Collection.update updateTypeReference gparam.Constraints
                Seq.iter updateCustomAttribute gparam.CustomAttributes

            Collection.update updateTypeReference t.Interfaces

            Seq.iter updateFieldReference t.Fields

            Seq.iter updatePropertyReference t.Properties

            Seq.iter updateEventReference t.Events

            Seq.iter updateMethodDefinition t.Methods

        Seq.iter updateTypeDefinition types