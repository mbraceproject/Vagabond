namespace Nessos.DistribFsi

    open System
    open System.IO

    open FsPickler

    type ISerializer =
        abstract Serialize<'T> : Stream -> 'T -> unit
        abstract Deserialize<'T> : Stream -> 'T

    type FsPicklerSerializer (?fsp : FsPickler) =
        let fsp = match fsp with None -> new FsPickler() | Some fsp -> fsp

        interface ISerializer with
            member __.Serialize<'T> s t = fsp.Serialize<'T>(s,t)
            member __.Deserialize<'T> s = fsp.Deserialize<'T>(s)
        

    type SerializationSupport private () =
        
        static let serializer : ISerializer option ref = ref None

        static let getSerializer () = 
            match !serializer with
            | None -> invalidOp "DistribFsi: no serializer has been registered."
            | Some s -> s

        static member RegisterSerializer(s : ISerializer) =
            lock serializer (fun () ->
                match !serializer with
                | Some _ -> invalidOp "DistribFsi: no serializer has already been registered."
                | None -> serializer := Some s)

        static member IsRegistered = serializer.Value.IsSome
        static member Serializer = getSerializer ()

        static member Pickle(o:obj) : byte [] =
            use m = new MemoryStream()
            getSerializer().Serialize<obj> m o
            m.ToArray()

        static member UnPickle(bytes:byte[]) : obj =
            use m = new MemoryStream(bytes)
            getSerializer().Deserialize<obj> m

        static member PickleToString(o:obj) : string =
            let bytes = SerializationSupport.Pickle o
            Convert.ToBase64String bytes

        static member UnPickleOfString(encoding:string) : obj =
            let bytes = Convert.FromBase64String encoding
            SerializationSupport.UnPickle bytes