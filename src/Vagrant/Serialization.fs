module internal Nessos.Vagrant.Serialization

    open System
    open System.IO
    open System.Text.RegularExpressions
    open System.Reflection

    open Nessos.FsPickler
    open Nessos.Vagrant

    type VagrantTypeNameConverter(stateF : unit -> DynamicAssemblyCompilerState) =

        interface ITypeNameConverter with
            member __.OfSerializedType(typeInfo : TypeInfo) = 
                let qname = typeInfo.AssemblyQualifiedName
                match stateF().DynamicAssemblies.TryFind qname with
                | None -> typeInfo
                | Some info ->
                    match info.TypeIndex.TryFind typeInfo.Name with
                    | None | Some (InNoSlice | InAllSlices) ->
                        raise <| new VagrantException(sprintf "type '%s' in dynamic assembly '%s' does not correspond to slice." typeInfo.Name qname)

                    | Some (InSpecificSlice slice) -> 
                        { typeInfo with AssemblyName = slice.Assembly.GetName().Name }
                    
            member __.ToDeserializedType(typeInfo : TypeInfo) =
                match stateF().TryGetDynamicAssemblyId typeInfo.AssemblyQualifiedName with
                | None -> typeInfo
                | Some (assemblyName,_) -> { typeInfo with AssemblyName = assemblyName }


    let mkFsPicklerInstance (registry : CustomPicklerRegistry option) (stateF : unit -> DynamicAssemblyCompilerState) =

        let tyConv = new VagrantTypeNameConverter(stateF) :> ITypeNameConverter

        let registry =
            match registry with
            | None -> let r = new CustomPicklerRegistry("Vagrant Custom Pickler") in r.SetTypeNameConverter tyConv ; r
            | Some r ->
                let tyConv =
                    match r.TypeNameConverter with
                    | None -> tyConv
                    | Some c ->
                        {
                            new ITypeNameConverter with
                                member __.OfSerializedType tI = tI |> tyConv.OfSerializedType |> c.OfSerializedType
                                member __.ToDeserializedType tI = tI |> c.ToDeserializedType |> tyConv.ToDeserializedType
                        }

                r.SetTypeNameConverter tyConv
                r

        new FsPickler(registry)