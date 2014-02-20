namespace Nessos.DistribFsi
    
    open System
    open System.IO

    open FsPickler

    type TypeInfo = FsPickler.TypeInfo
    and ITypepeNameConverter = FsPickler.ITypeNameConverter

    type ISerializer =
        abstract Serialize : Stream -> 'T -> unit
        abstract Deserialize : Stream -> 'T

    and ISerializerFactory =
        abstract Create : ITypeNameConverter -> ISerializer

    // a concrete implementation using FsPickler

    type FsPicklerSerializer(?fsp : FsPickler) =

        let fsp = match fsp with None -> new FsPickler() | Some fsp -> fsp
        
        member __.Pickler = fsp

        interface ISerializer with
            member __.Serialize<'T> (stream : Stream) (t : 'T) =
                fsp.Serialize(stream, t)

            member __.Deserialize<'T> (stream : Stream) =
                fsp.Deserialize<'T>(stream)

    and FsPicklerSerializerFactory (?registry : CustomPicklerRegistry, ?additionalTyConv : ITypeNameConverter) =

        static let compose (a : ITypeNameConverter) (b : ITypeNameConverter) =
            {
                new ITypeNameConverter with
                    member __.OfSerializedType tI = tI |> a.OfSerializedType |> b.OfSerializedType
                    member __.ToDeserializedType tI = tI |> b.ToDeserializedType |> a.ToDeserializedType
            }
        
        interface ISerializerFactory with
            member __.Create(fsiConv : ITypeNameConverter) =
                let registry = 
                    match registry with 
                    | None -> new CustomPicklerRegistry("Fsi Pickler") 
                    | Some r -> r

                let tyConv =
                    match additionalTyConv with
                    | None -> fsiConv
                    | Some tc -> compose fsiConv tc

                registry.SetTypeNameConverter tyConv

                let pickler = new FsPickler(registry)

                new FsPicklerSerializer(pickler) :> ISerializer