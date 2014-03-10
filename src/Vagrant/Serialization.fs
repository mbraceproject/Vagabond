module internal Nessos.Vagrant.Serialization

    open System
    open System.IO
    open System.Reflection

    open FsPickler

    open Nessos.Vagrant

    [<AbstractClass>]
    type DistribFsiNameConverter() =

        abstract DynamicAssemblyState : GlobalDynamicAssemblyState
        abstract TryGetDynamicAssemblyNameOfSlice : string -> string option

        interface ITypeNameConverter with
            member __.OfSerializedType(typeInfo : TypeInfo) = 
                let qname = typeInfo.AssemblyQualifiedName
                match __.DynamicAssemblyState.DynamicAssemblies.TryFind qname with
                | None -> typeInfo
                | Some info ->
                    match info.TypeIndex.TryFind typeInfo.Name with
                    | None -> failwithf "could not serialize type '%s' in dynamic assembly '%s'." typeInfo.Name qname
                    | Some a -> { typeInfo with AssemblyName = a.GetName().Name }
                    
            member __.ToDeserializedType(typeInfo : TypeInfo) =
                match __.TryGetDynamicAssemblyNameOfSlice typeInfo.AssemblyQualifiedName with
                | None -> typeInfo
                | Some assemblyName -> { typeInfo with AssemblyName = assemblyName }



    let mkFsPicklerInstance (registry : CustomPicklerRegistry option) (conv : DistribFsiNameConverter) =

        let registry =
            match registry with
            | None -> let r = new CustomPicklerRegistry("Vagrant Pickler") in r.SetTypeNameConverter conv ; r
            | Some r ->
                let tyConv =
                    match r.TypeNameConverter with
                    | None -> conv :> ITypeNameConverter
                    | Some c ->
                        let conv = conv :> ITypeNameConverter
                        {
                            new ITypeNameConverter with
                                member __.OfSerializedType tI = tI |> conv.OfSerializedType |> c.OfSerializedType
                                member __.ToDeserializedType tI = tI |> c.ToDeserializedType |> conv.ToDeserializedType
                        }

                r.SetTypeNameConverter tyConv
                r

        new FsPickler(registry)