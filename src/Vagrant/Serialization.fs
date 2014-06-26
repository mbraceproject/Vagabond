module internal Nessos.Vagrant.Serialization

    open System
    open System.IO
    open System.Text
    open System.Text.RegularExpressions
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.SliceCompilerTypes

    // temp extension method implementation ; should be added by next nuget release
    type AssemblyInfo with
        member aI.AssemblyQualifiedName =
            let sb = new StringBuilder()
            sb.Append aI.Name |> ignore
            match aI.Version with
            | null -> sb.Append(", Version=0.0.0.0")|> ignore
            | v -> sb.Append(", Version=").Append v |> ignore
            match aI.Culture with
            | null -> sb.Append(", Culture=neutral")  |> ignore
            | c -> sb.Append(", Culture=").Append c |> ignore
            match aI.PublicKeyToken with
            | null | "" -> sb.Append(", PublicKeyToken=null") |> ignore
            | pkt -> sb.Append(", PublicKeyToken=").Append(pkt) |> ignore

            sb.ToString()


    type VagrantTypeNameConverter(stateF : unit -> DynamicAssemblyCompilerState) =

        interface ITypeNameConverter with
            member __.OfSerializedType(typeInfo : TypeInfo) = 
                let qname = typeInfo.AssemblyInfo.AssemblyQualifiedName
                match stateF().DynamicAssemblies.TryFind qname with
                | None -> typeInfo
                | Some info ->
                    match info.TypeIndex.TryFind typeInfo.Name with
                    | None | Some (InNoSlice | InAllSlices) ->
                        raise <| new VagrantException(sprintf "type '%s' in dynamic assembly '%s' does not correspond to slice." typeInfo.Name qname)

                    | Some (InSpecificSlice slice) -> 
                        { typeInfo with AssemblyInfo = { typeInfo.AssemblyInfo with Name = slice.Assembly.GetName().Name } }
                    
            member __.ToDeserializedType(typeInfo : TypeInfo) =
                match stateF().TryGetDynamicAssemblyId typeInfo.AssemblyInfo.AssemblyQualifiedName with
                | None -> typeInfo
                | Some (assemblyName,_) -> { typeInfo with AssemblyInfo = { typeInfo.AssemblyInfo with Name = assemblyName } }


    let mkTypeNameConverter (tyConv' : ITypeNameConverter option) (stateF : unit -> DynamicAssemblyCompilerState) =

        let tyConv = new VagrantTypeNameConverter(stateF) :> ITypeNameConverter

        match tyConv' with
        | None -> tyConv
        | Some c ->
            {
                new ITypeNameConverter with
                    member __.OfSerializedType tI = tI |> tyConv.OfSerializedType |> c.OfSerializedType
                    member __.ToDeserializedType tI = tI |> c.ToDeserializedType |> tyConv.ToDeserializedType
            }