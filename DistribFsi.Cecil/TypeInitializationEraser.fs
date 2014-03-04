module internal Nessos.DistribFsi.TypeInitializationEraser

    open System
    open System.Reflection

    open Mono.Cecil
    open Mono.Cecil.Cil

    [<AutoOpen>]
    module private Helpers =

        let deserializerMethod = lazy(
            match typeof<Nessos.DistribFsi.SerializationSupport>.GetMethod("UnPickleOfString") with
            | null -> invalidOp "Could not resolve deserializer method."
            | m -> m)

        let allDefs = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static
        let allStatic = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static

        let cctorAttrs = 
            MethodAttributes.Private ||| MethodAttributes.Static ||| MethodAttributes.HideBySig ||| 
            MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName




    let eraseTypeInitializers (loadedAssembly : Assembly) (assemblyDefinition : AssemblyDefinition) =
        
        let eraseFromTypeDef (voidRef : TypeReference) (deserializerRef : MethodReference) (loadedType : Type) (typeDef : TypeDefinition) =
            // generic types will not be erased
            if typeDef.GenericParameters.Count > 0 then [] else

            let cctor = typeDef.Methods |> Seq.tryFind (fun m -> m.Name = ".cctor")
            let staticFields = typeDef.Fields |> Seq.filter (fun f -> f.IsStatic) |> Seq.toList

            if cctor.IsNone && staticFields.IsEmpty then []
            else
                let cctor = 
                    match cctor with
                    | Some cctor -> cctor
                    | None -> 
                        let cctor = new MethodDefinition(".cctor", cctorAttrs, voidRef)
                        typeDef.Methods.Add(cctor)
                        cctor

                let ilProc = cctor.Body.GetILProcessor()
                do ilProc.Body.Instructions.Clear()

                let generateUnPickleInstructionsForField (f : FieldDefinition) =
                    let loadedField = loadedType.GetField(f.Name, allStatic)
                    if loadedField = null then
                        failwithf "could not resolve field '%O::%s'" loadedType f.Name

                    try
                        // temporary solution for serializable assemblies
                        if typeof<Assembly>.IsAssignableFrom loadedField.FieldType then failwith "assembly not serializable!"

                        let value = loadedField.GetValue(null)
                        let pickle = Nessos.DistribFsi.SerializationSupport.PickleToString value

                        // sfield <- unbox<'T> (Unpickle pickle)
                        seq {
                            yield ilProc.Create(OpCodes.Ldstr, pickle)
                            yield ilProc.Create(OpCodes.Call, deserializerRef)
                            if f.FieldType.IsValueType then
                                yield ilProc.Create(OpCodes.Unbox_Any, f.FieldType)
                            else
                                yield ilProc.Create(OpCodes.Castclass, f.FieldType)

                            yield ilProc.Create(OpCodes.Stsfld, f)

                        } |> Seq.iter ilProc.Append

                        None
                    with e ->
                        // ok, pickling failed for this field; leave it uninitialized and report
                        Some (e, loadedField)

                let errors = staticFields |> List.choose generateUnPickleInstructionsForField

                ilProc.Append(ilProc.Create(OpCodes.Ret))

                errors

        let eraseFromModuleDef (moduleDef : ModuleDefinition) =
            let voidRef = moduleDef.Import typeof<Void>
            let deserializerRef = moduleDef.Import deserializerMethod.Value

            let rec eraseType (loadedDeclaringType : Type option) (typeDef : TypeDefinition) =
                let loadedType =
                    match loadedDeclaringType with
                    | None ->
                        let name = typeDef.FullName.Replace('/','.')
                        loadedAssembly.GetType(name, true)
                    | Some dt ->
                        let t = dt.GetNestedType(typeDef.Name, allDefs)
                        if t = null then failwith "Got null when resolving type '%s'." typeDef.FullName
                        t

                seq {
                    yield! eraseFromTypeDef voidRef deserializerRef loadedType typeDef
                    for nt in typeDef.NestedTypes do
                        yield! eraseType (Some loadedType) nt
                }

            moduleDef.Types |> Seq.filter (fun t -> t.Name <> "<Module>" ) |> Seq.collect (eraseType None) |> Seq.toList


        assemblyDefinition.Modules |> Seq.collect eraseFromModuleDef |> Seq.toList

