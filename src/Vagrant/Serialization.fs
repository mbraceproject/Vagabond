module internal Nessos.Vagrant.Serialization

    open System
    open System.IO
    open System.Text.RegularExpressions
    open System.Reflection

    open FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.SliceCompiler

    type VagrantTypeNameConverter(compiler : SliceCompilationServer) =



        interface ITypeNameConverter with
            member __.OfSerializedType(typeInfo : TypeInfo) = 
                let qname = typeInfo.AssemblyQualifiedName
                match compiler.State.DynamicAssemblies.TryFind qname with
                | None -> typeInfo
                | Some info ->
                    match info.TypeIndex.TryFind typeInfo.Name with
                    | None -> failwithf "Vagrant: type '%s' in dynamic assembly '%s' does not correspond to slice." typeInfo.Name qname
                    | Some a -> { typeInfo with AssemblyName = a.Assembly.GetName().Name }
                    
            member __.ToDeserializedType(typeInfo : TypeInfo) =
                match compiler.State.TryGetDynamicAssemblyName typeInfo.AssemblyQualifiedName with
                | None -> typeInfo
                | Some assemblyName -> { typeInfo with AssemblyName = assemblyName }



    let mkFsPicklerInstance (registry : CustomPicklerRegistry option) (compiler : SliceCompilationServer) =

        let tyConv = new VagrantTypeNameConverter(compiler) :> ITypeNameConverter

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