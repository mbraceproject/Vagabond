namespace Nessos.Vagabond

    open System
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.IO
    open System.Reflection
    open System.Runtime.Serialization
    open System.Security.Cryptography

    open Microsoft.FSharp.Control

    #nowarn "42"

    [<AutoOpen>]
    module internal Utils =

        let runsOnMono = lazy(Type.GetType("Mono.Runtime") <> null)

        /// Value or exception
        type Exn<'T> =
            | Success of 'T
            | Error of exn
        with
            /// evaluate, re-raising the exception if failed
            member e.Value =
                match e with
                | Success t -> t
                | Error e -> raise e

        module Exn =
            let catch (f : unit -> 'T) =
                try f () |> Success with e -> Error e

            let map (f : 'T -> 'S) (x : Exn<'T>) =
                match x with
                | Success x -> Success (f x)
                | Error e -> Error e

            let bind (f : 'T -> 'S) (x : Exn<'T>) =
                match x with
                | Success x -> try Success <| f x with e -> Error e
                | Error e -> Error e

        module Option =
            let filter (f : 'T -> bool) (x : 'T option) : 'T option = 
                match x with 
                | Some t when f t -> x
                | _ -> None

        let memoize f =
            let dict = new Dictionary<_,_>()
            fun x ->
                let ok,y = dict.TryGetValue x
                if ok then y
                else
                    let y = f x
                    dict.Add(x,y)
                    y

        let concurrentMemoize (f : 'a -> 'b) =
            let dict = new ConcurrentDictionary<'a,'b>()
            fun x -> dict.GetOrAdd(x, f)

        let tryConcurrentMemoize (f : 'a -> 'b option) : 'a -> 'b option =
            let dict = new ConcurrentDictionary<_,_>()
            fun x ->
                let ok,y = dict.TryGetValue x
                if ok then y
                else
                    match f x with
                    | None -> None
                    | Some _ as y ->
                        let _ = dict.TryAdd(x, y)
                        y

        let allBindings = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static

        /// try get assembly that is loaded in current appdomain

        let tryGetLoadedAssembly =
            let load fullName =
                let results =
                    System.AppDomain.CurrentDomain.GetAssemblies()
                    |> Array.filter (fun a -> a.FullName = fullName)

                match results with
                | [||] -> None
                | [|a|] -> Some a
                | _ ->
                    raise <| new VagabondException(sprintf "ran into duplicate assemblies of qualified name '%s'. This is not supported." fullName)

            tryConcurrentMemoize load

        /// try get assembly loaded in appdomain or load it now

        let tryLoadAssembly (fullName : string) =
            match tryGetLoadedAssembly fullName with
            | Some _ as a -> a
            | None ->
                try Some <| Assembly.Load(fullName)
                with :? FileNotFoundException | :? FileLoadException -> None

        // toplogical sorting for DAGs

        type Graph<'T> = ('T * 'T list) list

        let getTopologicalOrdering<'T when 'T : equality> (g : Graph<'T>) =
            let rec aux sorted (g : Graph<'T>) =
                if g.IsEmpty then sorted else

                match g |> List.tryFind (function (_,[]) -> true | _ -> false) with
                | None ->  failwith "internal error: dependency graph is not a DAG."
                | Some (t,_) ->
                    let g0 = g |> List.choose (fun (t0, ts) -> if t0 = t then None else Some(t0, List.filter ((<>) t) ts))
                    aux (t :: sorted) g0

            List.rev <| aux [] g



        type ReplyChannel<'T> internal (rc : AsyncReplyChannel<Exn<'T>>) =
            member __.Reply (t : 'T) = rc.Reply <| Success t
            member __.Reply (t : Exn<'T>) = rc.Reply t
            member __.ReplyWithError (e : exn) = rc.Reply <| Error e

        and MailboxProcessor<'T> with
            member m.PostAndReply (msgB : ReplyChannel<'R> -> 'T) =
                m.PostAndReply(fun ch -> msgB (new ReplyChannel<_>(ch))).Value

            member m.PostAndAsyncReply (msgB : ReplyChannel<'R> -> 'T) = async {
                let! result = m.PostAndAsyncReply(fun ch -> msgB(new ReplyChannel<_>(ch)))
                return result.Value
            }


        and MailboxProxessor =
            static member Stateful (init, processF : 'State -> 'Message -> Async<'State>, ?ct) =
                let rec loop state (self : MailboxProcessor<'Message>) = async {
                    let! message = self.Receive()
                    let! state' = processF state message
                    return! loop state' self
                }

                new MailboxProcessor<_>(loop init, ?cancellationToken = ct)


        /// computes a unique assembly identifier

        type AssemblyIdGenerator private () =
            static let idCache = new ConcurrentDictionary<Assembly, AssemblyId> ()
            static let hashAlgorithm = SHA256Managed.Create()
            static let hostId = Guid.NewGuid().ToByteArray()

            static let computeHash (assembly : Assembly) =
                let hash =
                    if assembly.IsDynamic then
                        let this = BitConverter.GetBytes(assembly.GetHashCode())
                        Array.append hostId this
                    elif File.Exists(assembly.Location) then
                        use fs = new FileStream(assembly.Location, FileMode.Open, FileAccess.Read)
                        hashAlgorithm.ComputeHash(fs)
                    else
                        raise <| new VagabondException(sprintf "could not resolve location for '%O'." assembly)

                { FullName = assembly.FullName ; ImageHash = hash }

            static member GetAssemblyId (assembly : Assembly) = idCache.GetOrAdd(assembly, computeHash)


        type Assembly with 
            member a.AssemblyId = AssemblyIdGenerator.GetAssemblyId a

        type AssemblyId with
            member id.CanBeResolvedLocally (policy : AssemblyLoadPolicy) =
                if policy.HasFlag AssemblyLoadPolicy.ResolveAll then true
                elif policy.HasFlag AssemblyLoadPolicy.ResolveStrongNames then 
                    id.IsStrongAssembly
                else
                    false

        type AssemblyPackage with
            static member Empty (id : AssemblyId) =
                { Id = id ; Image = None ; Symbols = None ; StaticInitializer = None }

        [<RequireQualifiedAccess>]
        module Convert =
        
            open System.Text
            open System.IO
            open System.Collections.Generic

            // taken from : http://www.atrevido.net/blog/PermaLink.aspx?guid=debdd47c-9d15-4a2f-a796-99b0449aa8af
            let private encodingIndex = "qaz2wsx3edc4rfv5tgb6yhn7ujm8k9lp"
            let private inverseIndex = encodingIndex |> Seq.mapi (fun i c -> c,i) |> dict

            /// convert bytes to base-32 string: useful for file names in case-insensitive file systems
            let toBase32String(bytes : byte []) =
                let b = new StringBuilder()
                let mutable hi = 5
                let mutable idx = 0uy
                let mutable i = 0
                
                while i < bytes.Length do
                    // do we need to use the next byte?
                    if hi > 8 then
                        // get the last piece from the current byte, shift it to the right
                        // and increment the byte counter
                        idx <- bytes.[i] >>> (hi - 5)
                        i <- i + 1
                        if i <> bytes.Length then
                            // if we are not at the end, get the first piece from
                            // the next byte, clear it and shift it to the left
                            idx <- ((bytes.[i] <<< (16 - hi)) >>> 3) ||| idx

                        hi <- hi - 3
                    elif hi = 8 then
                        idx <- bytes.[i] >>> 3
                        i <- i + 1
                        hi <- hi - 3
                    else
                        // simply get the stuff from the current byte
                        idx <- (bytes.[i] <<< (8 - hi)) >>> 3
                        hi <- hi + 5

                    b.Append (encodingIndex.[int idx]) |> ignore

                b.ToString ()