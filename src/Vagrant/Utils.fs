module internal Nessos.Vagrant.Utils

    open System
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.IO
    open System.Reflection
    open System.Runtime.Serialization
    open System.Security.Cryptography

    open Microsoft.FSharp.Control

    open Nessos.Vagrant.SliceCompilerTypes

    let inline raise (e : System.Exception) = (# "throw" e : 'T #)

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
                raise <| new VagrantException(sprintf "ran into duplicate assemblies of qualified name '%s'. This is not supported." fullName)

        tryConcurrentMemoize load

    /// try get assembly loaded in appdomain or load it now

    let tryLoadAssembly (fullName : string) =
        match tryGetLoadedAssembly fullName with
        | Some _ as a -> a
        | None ->
            try Some <| Assembly.Load(fullName)
            with :? FileNotFoundException | :? FileLoadException -> None

    /// computes a unique assembly identifier

    let computeAssemblyId : Assembly -> AssemblyId =
        let hashAlgorithm = SHA256Managed.Create()
        let hostId = Guid.NewGuid().ToByteArray()
        let compute (assembly : Assembly) =
            let hash =
                if assembly.IsDynamic then
                    let this = BitConverter.GetBytes(assembly.GetHashCode())
                    Array.append hostId this
                else
                    use fs = new FileStream(assembly.Location, FileMode.Open, FileAccess.Read)
                    hashAlgorithm.ComputeHash(fs)

            { FullName = assembly.FullName ; ImageHash = hash }

        concurrentMemoize compute

    type Assembly with member a.AssemblyId = computeAssemblyId a

    let allBindings = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static

    type DynamicAssemblyState with
        member s.HasFreshTypes =
            let currentTypeCount =
                if not runsOnMono.Value then
                    s.DynamicAssembly.GetTypes().Length
                else
                    // mono needs different approach since Assembly.GetTypes() only returns top-level types
                    let count = ref 0
                    let rec countTypes (types : Type []) =
                        count := !count + types.Length
                        for t in types do
                            countTypes <| t.GetNestedTypes(BindingFlags.NonPublic ||| BindingFlags.Public)

                    countTypes <| s.DynamicAssembly.GetTypes()
                    !count

            let compiledTypeCount = s.TypeIndex.Count
            currentTypeCount > compiledTypeCount


    // toplogical sorting for DAGs

    type Graph<'T> = ('T * 'T list) list

    let getTopologicalOrdering<'T when 'T : equality> (g : Graph<'T>) =
        let rec aux sorted (g : Graph<'T>) =
            if g.IsEmpty then sorted else

            match g |> List.tryFind (function (_,[]) -> true | _ -> false) with
            | None -> raise <| new VagrantException("internal error: dependency graph is not a DAG.")
            | Some (t,_) ->
                let g0 = g |> List.choose (fun (t0, ts) -> if t0 = t then None else Some(t0, List.filter ((<>) t) ts))
                aux (t :: sorted) g0

        aux [] g



    /// A stateful agent implementation with readable inner state

    type StatefulActor<'State, 'Input, 'Output>(init : 'State, f : 'State -> 'Input -> 'State * 'Output) =
        
        let stateRef = ref init

        let cts = new System.Threading.CancellationTokenSource()

        let rec behaviour (state : 'State) (mailbox : MailboxProcessor<'Input * AsyncReplyChannel<Exn<'Output>>>) =
            async {
                let! input, rc = mailbox.Receive()

                let state, reply = 
                    try 
                        let state, output = f state input
                        state, Success output
                    with e ->
                        state, Error e

                // published state must be updated *before* replying
                stateRef := state
                rc.Reply reply

                return! behaviour state mailbox
            }

        let agent = MailboxProcessor.Start (behaviour init, cts.Token)

        member __.CurrentState = !stateRef
        member __.PostAndReply input = agent.PostAndReply(fun ch -> input,ch).Value

        member __.Dispose () = cts.Cancel()

        interface IDisposable with
            member __.Dispose () = cts.Cancel()


    let mkStatefulActor (init : 'S) (f : 'S -> 'I -> 'S * 'O) = new StatefulActor<_,_,_>(init, f)



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