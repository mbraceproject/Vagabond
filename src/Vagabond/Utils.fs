namespace Nessos.Vagabond

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Reflection
open System.Runtime.Serialization
open System.Threading
open System.Threading.Tasks
open System.Text.RegularExpressions
open System.Security.Cryptography

open Microsoft.FSharp.Control

#nowarn "42"

[<AutoOpen>]
module internal Utils =

    let runsOnMono = lazy(Type.GetType("Mono.Runtime") <> null)

    let private remoteStackTraceField : FieldInfo =
        let bfs = BindingFlags.NonPublic ||| BindingFlags.Instance
        match typeof<System.Exception>.GetField("remote_stack_trace", bfs) with
        | null ->
            match typeof<System.Exception>.GetField("_remoteStackTraceString", bfs) with
            | null -> null
            | f -> f
        | f -> f

    // bad implementation: would be safer using ExceptionDispatchInfo but would break compatibility with 4.0
    let inline reraise' (e : #exn) =
        match remoteStackTraceField with
        | null -> ()
        | f ->
            let trace = e.StackTrace
            if not <| String.IsNullOrEmpty trace then f.SetValue(e, trace + System.Environment.NewLine)
            
        raise e

    /// Value or exception
    type Exn<'T> =
        | Success of 'T
        | Error of exn
    with
        /// evaluate, re-raising the exception if failed
        member inline e.Value =
            match e with
            | Success t -> t
            | Error e -> reraise' e

    module Exn =
        let inline catch (f : unit -> 'T) =
            try f () |> Success with e -> Error e

        let inline protect f t = try f t |> Success with e -> Error e
        let inline protect2 f t s = try f t s |> Success with e -> Error e

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

    module Choice =
        let split (inputs : Choice<'T,'S> []) =
            let ts,ss = ResizeArray<'T> (), ResizeArray<'S> ()
            for i = 0 to inputs.Length - 1 do
                match inputs.[i] with
                | Choice1Of2 t -> ts.Add t
                | Choice2Of2 s -> ss.Add s

            ts.ToArray(), ss.ToArray()


    type IDictionary<'K, 'V> with
        member d.TryFind(k : 'K) =
            let ok,v = d.TryGetValue k
            if ok then Some v
            else None

    type Async =

        /// <summary>
        ///     Runs the asynchronous computation and awaits its result.
        ///     Preserves original stacktrace for any exception raised.
        /// </summary>
        /// <param name="workflow">Workflow to be run.</param>
        /// <param name="cancellationToken">Optioncal cancellation token.</param>
        static member RunSync(workflow : Async<'T>, ?cancellationToken) =
            let tcs = new TaskCompletionSource<Choice<'T,exn,OperationCanceledException>>()
            let inline commit f r = tcs.SetResult(f r)
            let _ = 
                ThreadPool.QueueUserWorkItem(fun _ ->
                    Async.StartWithContinuations(workflow, 
                        commit Choice1Of3, commit Choice2Of3, commit Choice3Of3, 
                        ?cancellationToken = cancellationToken))

            match tcs.Task.Result with
            | Choice1Of3 t -> t
            | Choice2Of3 e -> reraise' e
            | Choice3Of3 e -> raise e

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

    /// Attempt to compute a topological sorting for graph if DAG,
    /// If not DAG returns the reduced DAG for further debugging
    let tryGetTopologicalOrdering<'T when 'T : equality> (g : Graph<'T>) : Choice<'T list, Graph<'T>> =
        let rec aux sorted (g : Graph<'T>) =
            if g.IsEmpty then Choice1Of2 (List.rev sorted) else

            match g |> List.tryFind (function (_,[]) -> true | _ -> false) with
            | None -> Choice2Of2 g // not a DAG, return reduced graph
            | Some (t,_) ->
                let g0 = g |> List.choose (fun (t0, ts) -> if t0 = t then None else Some(t0, List.filter ((<>) t) ts))
                aux (t :: sorted) g0

        aux [] g

    type ReplyChannel<'T> internal (rc : AsyncReplyChannel<Exn<'T>>) =
        member __.Reply (t : 'T) = rc.Reply <| Success t
        member __.Reply (t : Exn<'T>) = rc.Reply t
        member __.ReplyWithError (e : exn) = rc.Reply <| Error e

    and MailboxProcessor<'T> with
        member m.PostAndAsyncReply (msgB : ReplyChannel<'R> -> 'T) = async {
            let! result = m.PostAndAsyncReply(fun ch -> msgB(new ReplyChannel<_>(ch)))
            return result.Value
        }

        member m.PostAndReply (msgB : ReplyChannel<'R> -> 'T) =
            m.PostAndAsyncReply msgB |> Async.RunSync

    and MailboxProxessor =
        static member Stateful (init, processF : 'State -> 'Message -> Async<'State>, ?ct) =
            let rec loop state (self : MailboxProcessor<'Message>) = async {
                let! message = self.Receive()
                let! state' = processF state message
                return! loop state' self
            }

            new MailboxProcessor<_>(loop init, ?cancellationToken = ct)

    /// Strips invalid character from a candidate filename
    let stripInvalidFileChars =
        let invalidChars = new String(Path.GetInvalidFileNameChars()) |> Regex.Escape
        let regex = new Regex(sprintf "[%s]" invalidChars, RegexOptions.Compiled)
        fun (fileName : string) -> regex.Replace(fileName, "")

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

    module Map =
        
        /// <summary>
        ///     add multiple key-value pairs to map.
        /// </summary>
        /// <param name="kvs">Input key-value pairs.</param>
        /// <param name="map">Input map.</param>
        let addMany (map : Map<'K, 'V>) (kvs : seq<'K * 'V>) =
            let mutable map = map
            for (k,v) in kvs do
                map <- map.Add(k,v)
            map

    type AsyncBuilder with
        member ab.Bind(t : Task<'T>, g : 'T -> Async<'S>) = ab.Bind(Async.AwaitTask t, g)
        member ab.Bind(t : Task, g : unit -> Async<'S>) = ab.Bind(t.ContinueWith ignore, g)