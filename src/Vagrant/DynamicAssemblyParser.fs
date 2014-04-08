module internal Nessos.Vagrant.DynamicAssemblyParser

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Runtime.InteropServices

    open Mono.Reflection
    open Mono.Cecil
    open Mono.Cecil.Cil

    open Nessos.Vagrant

    [<AutoOpen>]
    module private Utils =

        let notImpl<'T> = raise <| new NotImplementedException()

        [<Literal>]
        let allDeclarations = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static

        let opCodes =
            typeof<OpCodes>
                .GetFields(BindingFlags.Static ||| BindingFlags.Public)
                |> Seq.map (fun f -> f.GetValue(null) :?> OpCode)
                |> Seq.toArray

        let initAssemblyDefinition (assembly : Assembly) =
            let name = assembly.GetName()
            let nameDef = new AssemblyNameDefinition(name.Name, name.Version)
            let assemblyDef = AssemblyDefinition.CreateAssembly(nameDef, assembly.ManifestModule.Name, ModuleKind.Dll)
            assemblyDef.MainModule.Runtime <- TargetRuntime.Net_4_0
            assemblyDef

        let shouldMapBody (m : MethodBase) (mdef : MethodDefinition) =
            mdef.HasBody && m.GetMethodBody() <> null

        let toByteArray (t : Type, obj : obj) =
            let size = Marshal.SizeOf t
            let data = Array.zeroCreate<byte> size
            let ptr = Marshal.AllocHGlobal size

            Marshal.StructureToPtr (obj, ptr, fDeleteOld = true)
            Marshal.Copy(ptr, data, 0, size)
            Marshal.FreeHGlobal ptr

            data            

        let getInitialValue (fI : FieldInfo) =
            if not fI.IsStatic then
                raise <| new NotSupportedException()

            match fI.GetValue(null) with
            | null -> [||]
            | value ->
                let t = value.GetType()
                if not t.IsValueType || t.IsPrimitive then
                    raise <| new NotSupportedException()

                toByteArray (t, value)

        let offsetToInstruction (offset : int, instructions : seq<Instruction>, mDef : MethodDefinition) =
            match instructions |> Seq.tryFindIndex (fun instr -> instr.Offset = offset) with
            | None -> null
            | Some idx -> mDef.Body.Instructions.[idx]


    let parseDynamicAssembly makePublic (ignoreF : Type -> bool) (remapF : Type -> Type) (assembly : Assembly) =

        // section 1: initialize assembly definition
        let assemblyDef = initAssemblyDefinition assembly
        let mainModuleDef = assemblyDef.MainModule

        let rec mapType (declaringType : TypeDefinition option) (t : Type) = notImpl<unit>

        and mapMethod (tDef : TypeDefinition) (m : MethodBase) = notImpl<unit>

        and mapPInvokeInfo (methodDef : MethodDefinition) (m : MethodBase) = notImpl<unit>

        and mkModuleRef (name : string) = notImpl<ModuleReference>

        and mapOverrides (m : MethodBase) (mdef : MethodDefinition) = notImpl<unit>

        and mapField (tDef : TypeDefinition) (fI : FieldInfo) = notImpl<unit>

        and mapProperty (p : PropertyInfo) (pDef : PropertyDefinition) = notImpl<unit>

        and mkPropertyDef (p : PropertyInfo) (tDef : TypeDefinition) = notImpl<PropertyDefinition>

        and mapEvent (e : EventInfo) (eDef : EventDefinition) = notImpl<unit>

        and mkEventDef (e : EventInfo) (tDef : TypeDefinition) = notImpl<EventDefinition>

        and mapMethodBody (m : MethodBase) (mDef : MethodDefinition) = notImpl<unit>

        and mapInstructions (m : MethodBase) (mDef : MethodDefinition) = notImpl<unit>

        and mapVariables (m : MethodBase) (mDef : MethodDefinition) = notImpl<unit>

        and mapExceptions (m : MethodBase) (mDef : MethodDefinition) = notImpl<unit>

        and mkMethodDefinition (m : MethodBase) (tDef : TypeDefinition) = notImpl<MethodDefinition>

        and mapParameter (m : MethodDefinition) (p : ParameterInfo) = notImpl<unit>

        and mkFieldDefinition (f : FieldInfo) (tDef : TypeDefinition) = notImpl<FieldDefinition>

        and mkTypeDefinition (t : Type) (declaringType : TypeDefinition option) = notImpl<TypeDefinition>

        and mapGenericParameters (gparams : Type []) (owner : IGenericParameterProvider) = notImpl<seq<GenericParameter>>

        and mkParameterDefinition (instr : Instruction) (mDef : MethodDefinition) = notImpl<ParameterDefinition>

        and mkVariableDefinition (instr : Instruction) (mDef : MethodDefinition) = notImpl<VariableDefinition>

        and mkOpCode (instr : Instruction) = notImpl<OpCode>

        and mkTypeReference (t : Type) = notImpl<TypeReference>

        and mkTypeReferenceWithTypeContext (t : Type) (ctx : TypeReference) = notImpl<TypeReference>

        and mkTypeReferenceWithMethodContext (t : Type) (ctx : MethodReference) = notImpl<TypeReference>

        and mkFieldReference (f : FieldInfo) (ctx : MethodReference) = notImpl<FieldReference>

        and mkMethodReference (f : FieldInfo) (ctx : MethodReference) = notImpl<MethodReference>

        and mapGenericArguments (m : MethodReference) = notImpl<unit>

        and mapTypeReference (t : TypeReference) = notImpl<TypeReference>

        and mapElementType (t : TypeReference) = notImpl<unit>

        and mapCustomAttributes (source : System.Reflection.ICustomAttributeProvider) (target : Mono.Cecil.ICustomAttributeProvider) = notImpl<unit>

        and mkCustomAttributeArgument (argument : CustomAttributeTypedArgument) = notImpl<CustomAttributeArgument>

        and mapCustomAttributeValue (argument : CustomAttributeTypedArgument) = notImpl<unit>

        ()