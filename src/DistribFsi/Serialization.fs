module internal Nessos.DistribFsi.Serialization

    open Nessos.DistribFsi

    open System
    open System.Reflection
    open System.Text.RegularExpressions

    open FsPickler

    type ISynSerializerProvider =
        abstract Serialize : obj -> byte []
        abstract CheckSerializable : Type -> bool
        abstract SynDeserializerPath : string list
        abstract DeserializerAssembly : Assembly


    type FsiTypeNameConverter =
        {
            FsiAssemblyName : string // FSI-ASSEMBLY
            CompiledAssemblyPattern : Regex // matches compiled interaction assembly names
            ModuleIndex : ReadOnly<Map<string, FsiCompiledAssembly>> // "FSI_XXXX" -> FsiCompiledAssembly
        }
    with
        interface ITypeNameConverter with
            member tc.OfSerializedType (typeInfo : TypeInfo) =
                if typeInfo.AssemblyName = tc.FsiAssemblyName then
                    let moduleName = typeInfo.Name.Split([|'.';'+'|]).[0]
                    let compiledAssembly = tc.ModuleIndex.Value.[moduleName]

                    { typeInfo with AssemblyName = compiledAssembly.Name }
                else
                    typeInfo

            member tc.ToDeserializedType (typeInfo : TypeInfo) =
                if tc.CompiledAssemblyPattern.IsMatch typeInfo.AssemblyName then
                    { typeInfo with AssemblyName = tc.FsiAssemblyName }
                else
                    typeInfo


    type FsiSerializer(tc : FsiTypeNameConverter) =

        static let resolveDeserializerInfo () =
            let deserializerM =
                match typeof<Nessos.DistribFsi.DeserializationSupport>.GetMethod("Deserialize") with
                | null -> invalidOp "DistribFsi: Could not locate dserialization method."
                | m when m.IsStatic && not m.DeclaringType.IsGenericType && m.ReturnType.IsGenericParameter 
                            && m.GetParameters() |> Array.map (fun p -> p.ParameterType) = [|typeof<byte []>|] -> m
                | _ -> invalidOp "DistribFsi: Invalid deserialization method."

            let assembly = deserializerM.DeclaringType.Assembly
            let path = [ yield! deserializerM.DeclaringType.FullName.Split([|'.' ; '+'|]) ; yield deserializerM.Name ]
            assembly, path

        let deserializerAssembly, synDeserializerPath = resolveDeserializerInfo ()

        let customRegistry = new CustomPicklerRegistry("FSI types Serializer")
        do customRegistry.SetTypeNameConverter tc
        let pickler = new FsPickler(customRegistry)

        member __.Pickler = pickler

        interface ISynSerializerProvider with
            member __.Serialize (o : obj) = pickler.Pickle<obj> o
            member __.CheckSerializable (t : Type) = pickler.IsSerializableType t
            member __.SynDeserializerPath = synDeserializerPath
            member __.DeserializerAssembly = deserializerAssembly