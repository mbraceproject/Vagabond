module internal Nessos.DistribFsi.Serialization

    open System
    open System.IO
    open System.Text.RegularExpressions
    open System.Reflection

    open FsPickler

    open Nessos.DistribFsi

    [<AbstractClass>]
    type DistribFsiNameConverter() as self =

        let mkQualifiedName (t : TypeInfo) = 
            let sb = new System.Text.StringBuilder()
            let inline add (x:string) = sb.Append x |> ignore
            add t.Name
            add ", Version="
            add (if String.IsNullOrEmpty t.Version then "0.0.0.0" else t.Version)
            add ", Culture="
            add (if String.IsNullOrEmpty t.Culture then "neutral" else t.Culture)
            add ", PublicKeyToken="
            if t.PublicKeyToken.Length = 0 then add "null"
            else
                for b in t.PublicKeyToken do
                    add <| sprintf "%02x" b

            sb.ToString()

        let assemblyRegex = Regex(sprintf "^(.*)_%s_[0-9]*$" self.DynamicAssemblyState.ServerId)

        abstract DynamicAssemblyState : GlobalDynamicAssemblyState

        interface ITypeNameConverter with
            member __.OfSerializedType(typeInfo : TypeInfo) = 
                let qname = mkQualifiedName typeInfo
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
    //

//        let fsPickler = 
                



//    type ISerializer =
//        abstract Serialize<'T> : Stream -> 'T -> unit
//        abstract Deserialize<'T> : Stream -> 'T
//
//    type FsPicklerSerializer (?fsp : FsPickler) =
//        let fsp = match fsp with None -> new FsPickler() | Some fsp -> fsp
//
//        interface ISerializer with
//            member __.Serialize<'T> s t = fsp.Serialize<'T>(s,t)
//            member __.Deserialize<'T> s = fsp.Deserialize<'T>(s)
//        
//
//    type SerializationSupport private () =
//        
//        static let serializer : ISerializer option ref = ref None
//
//        static let getSerializer () = 
//            match !serializer with
//            | None -> invalidOp "DistribFsi: no serializer has been registered."
//            | Some s -> s
//
//        static member RegisterSerializer(s : ISerializer) =
//            lock serializer (fun () ->
//                match !serializer with
//                | Some _ -> invalidOp "DistribFsi: no serializer has already been registered."
//                | None -> serializer := Some s)
//
//        static member IsRegistered = serializer.Value.IsSome
//        static member Serializer = getSerializer ()
//
//        static member Pickle(o:obj) : byte [] =
//            use m = new MemoryStream()
//            getSerializer().Serialize<obj> m o
//            m.ToArray()
//
//        static member UnPickle(bytes:byte[]) : obj =
//            use m = new MemoryStream(bytes)
//            getSerializer().Deserialize<obj> m
//
//        static member PickleToString(o:obj) : string =
//            let bytes = SerializationSupport.Pickle o
//            Convert.ToBase64String bytes
//
//        static member UnPickleOfString(encoding:string) : obj =
//            let bytes = Convert.FromBase64String encoding
//            SerializationSupport.UnPickle bytes