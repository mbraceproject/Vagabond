module internal Nessos.DistribFsi.Serialization

    open System
    open System.IO
    open System.Text.RegularExpressions
    open System.Reflection

    open FsPickler

    open Nessos.DistribFsi

    [<AbstractClass>]
    type DistribFsiNameConverter() as self =

        let assemblyRegex = Regex(sprintf "^(.*)_%s_[0-9]*$" self.DynamicAssemblyState.ServerId)

        abstract DynamicAssemblyState : GlobalDynamicAssemblyState

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
                let m = assemblyRegex.Match(typeInfo.AssemblyName)
                if m.Success then
                    let assemblyName = m.Groups.[1].Value
                    { typeInfo with AssemblyName = assemblyName }
                else
                    typeInfo



    let mkFsPicklerInstance (registry : CustomPicklerRegistry option) (conv : DistribFsiNameConverter) =

        let registry =
            match registry with
            | None -> let r = new CustomPicklerRegistry("DistribFsi Pickler") in r.SetTypeNameConverter conv ; r
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