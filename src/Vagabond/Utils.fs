namespace MBrace.Vagabond

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Reflection
#if !NETSTANDARD
open System.Runtime.Loader
#endif
open System.Threading
open System.Threading.Tasks
open System.Text.RegularExpressions
open System.Runtime.ExceptionServices

open Microsoft.FSharp.Control

#nowarn "42"

[<AutoOpen>]
module internal Utils =

    let runsOnMono = lazy(Type.GetType("Mono.Runtime") <> null)
    let isNetCoreApp = System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription.StartsWith ".NET Core"

    let inline reraise' (e : #exn) : 'T = ExceptionDispatchInfo.Capture(e).Throw() ; Unchecked.defaultof<'T>

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
        ///     Asynchronously raises an exception.
        /// </summary>
        /// <param name="e">Exception to be raised.</param>
        static member Raise(e : exn) = Async.FromContinuations(fun (_,ec,_) -> ec e)

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


    type RetryPolicy = exn * int -> TimeSpan option

    type Retry =
        
        /// <summary>
        ///     Drives a retriable operation with provided retry policy.
        /// </summary>
        /// <param name="retryPolicy">Takes exception and number of failures returning a retry decision: either give up or retry after a given delay.</param>
        /// <param name="operation">Input retriable operation</param>
        static member Retry (retryPolicy : RetryPolicy) (operation : unit -> 'T) =
            let rec aux (retries : int) =
                let result = try operation () |> Choice1Of2 with e -> Choice2Of2 e
                match result with
                | Choice1Of2 t -> t
                | Choice2Of2 e ->
                    match retryPolicy (e, retries) with
                    | None -> reraise' e
                    | Some ts ->
                        Thread.Sleep (int ts.TotalMilliseconds)
                        aux (retries + 1)

            aux 1

        /// <summary>
        ///     Asynchronously drives a retriable operation with provided retry policy.
        /// </summary>
        /// <param name="retryPolicy">Takes exception and number of failures returning a retry decision: either give up or retry after a given delay.</param>
        /// <param name="operation">Input retriable operation</param>
        static member RetryAsync (retryPolicy : RetryPolicy) (operation : Async<'T>) = async {
            let rec aux (retries : int) = async {
                let! result = Async.Catch operation
                match result with
                | Choice1Of2 t -> return t
                | Choice2Of2 e ->
                    match retryPolicy (e, retries) with
                    | None -> return! Async.Raise e
                    | Some ts ->
                        do! Async.Sleep (int ts.TotalMilliseconds)
                        return! aux (retries + 1)
            }

            return! aux 1
        }
        

    let hset (ts : seq<'T>) = new HashSet<'T>(ts)

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

    let currentLoadContext =
        let asm = Assembly.GetExecutingAssembly()
        AssemblyLoadContext.GetLoadContext asm

    /// try get assembly that is loaded in current assembly load context
    let tryGetLoadedAssembly =
        let load fullName =
            // first, query AppDomain for loaded assembly of given qualified name
            let results =
                currentLoadContext.Assemblies
                |> Seq.filter (fun a -> a.FullName = fullName)
                |> Seq.toArray

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
            try Some <| currentLoadContext.LoadFromAssemblyName(AssemblyName fullName)
            with :? FileNotFoundException | :? FileLoadException -> None


    /// registers an assembly resolution handler based on AppDomain lookups;
    /// this is needed since assembly lookups often fail when loaded at runtime.
    let registerAssemblyResolutionHandler () = 
        System.AppDomain.CurrentDomain.add_AssemblyResolve <|
            new ResolveEventHandler (fun _ args -> defaultArg (tryGetLoadedAssembly args.Name) null)

    /// Directed graph representation
    type Graph<'T> = ('T * 'T list) list

    module Graph =

        /// <summary>
        ///     Maps directed graph to isomorphic instance of relabeled nodes.
        /// </summary>
        /// <param name="f">Mapper function.</param>
        /// <param name="graph">Input graph.</param>
        let map (f : 'T -> 'S) (graph : Graph<'T>) : Graph<'S> =
            graph |> List.map (fun (n, edges) -> f n, List.map f edges)

        /// <summary>
        ///     Filters nodes (and adjacent edges) that satisfy the provided predicate.
        /// </summary>
        /// <param name="f">Node filter function.</param>
        /// <param name="graph">Input directed graph.</param>
        let filterNode (f : 'T -> bool) (graph : Graph<'T>) : Graph<'T> =
            graph |> List.choose(fun (n, edges) -> if f n then Some(n, List.filter f edges) else None)

        /// <summary>
        ///     Filters directed edges from graph that satisfy provided predicate.
        /// </summary>
        /// <param name="f">Directed edge filter predicate.</param>
        /// <param name="graph">Input directed graph.</param>
        let filterEdge (f : 'T -> 'T -> bool) (graph : Graph<'T>) : Graph<'T> =
            graph |> List.map (fun (n, edges) -> (n, List.filter (fun e -> f n e) edges))

        /// Attempt to compute a topological sorting for graph if DAG,
        /// If not DAG returns a cycle within the graph for further debugging.
        let tryGetTopologicalOrdering<'T when 'T : equality> (g : Graph<'T>) : Choice<'T list, 'T list> =
            let locateCycle (g : Graph<'T>) =
                let d = dict g
                let rec tryFindCycleInPath (path : 'T list) (acc : 'T list) (t : 'T) =
                    match path with
                    | [] -> None
                    | h :: _ when h = t -> Some (h :: acc)
                    | h :: tl -> tryFindCycleInPath tl (h :: acc) t

                let rec walk (path : 'T list) (t : 'T) =
                    match tryFindCycleInPath path [] t with
                    | Some _ as cycle -> cycle
                    | None -> d.[t] |> List.tryPick (walk (t :: path))

                g |> List.head |> fst |> walk [] |> Option.get

            let rec aux sorted (g : Graph<'T>) =
                if List.isEmpty g then Choice1Of2 (List.rev sorted) else

                match g |> List.tryFind (function (_,[]) -> true | _ -> false) with
                | None -> Choice2Of2 (locateCycle g) // not a DAG, detect and report a cycle in graph
                | Some (t,_) ->
                    let g0 = g |> filterNode ((<>) t)
                    aux (t :: sorted) g0

            aux [] g

    type ReplyChannel<'T> internal (rc : AsyncReplyChannel<Exn<'T>>) =
        member __.Reply (t : 'T) = rc.Reply <| Success t
        member __.Reply (t : Exn<'T>) = rc.Reply t
        member __.ReplyWithError (e : exn) = rc.Reply <| Error e

    and MailboxProcessor<'T> with
        member m.PostAndAsyncReply (msgB : ReplyChannel<'R> -> 'T) : Async<'R> = async {
            let! result = m.PostAndAsyncReply(fun ch -> msgB(new ReplyChannel<_>(ch)))
            return result.Value
        }

        member m.PostAndReply (msgB : ReplyChannel<'R> -> 'T) : 'R =
            m.PostAndReply(fun ch -> msgB(new ReplyChannel<_>(ch))).Value

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
        // this is just the hardcoded output of System.IO.Path.GetInvalidFileNameChars()
        // when run on windows. Mono/Unix implementations use a smaller subset of chars
        // which can result in non-deterministic path generations in heterogeneous systems
        // c.f. http://stackoverflow.com/q/2178173 
        // and  https://github.com/mono/mono/blob/059d656a9b851e90585e3a9f70a7c02e4cc0354e/mcs/class/corlib/System.IO/Path.cs#L549
        let invalidChars = 
            [| '"'; '<'; '>'; '|'; '\000'; '\001'; '\002'; '\003'; '\004'; '\005'; '\006';
               '\007'; '\b'; '\009'; '\010'; '\011'; '\012'; '\013'; '\014'; '\015';
               '\016'; '\017'; '\018'; '\019'; '\020'; '\021'; '\022'; '\023'; '\024';
               '\025'; '\026'; '\027'; '\028'; '\029'; '\030'; '\031'; ':'; '*'; '?';
               '\\'; '/' |]

        let escaped = new String(invalidChars) |> Regex.Escape
        let regex = new Regex(sprintf "[%s]+" escaped, RegexOptions.Compiled)
        fun (fileName : string) -> regex.Replace(fileName, "_")

    /// gets symbols file path for given cached assembly
    let getSymbolsPath path = 
        if runsOnMono.Value then Path.ChangeExtension(path, ".mdb") 
        else Path.ChangeExtension(path, ".pdb")

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


    /// Assembly classifier active pattern
    let (|StaticAssembly|DynamicAssembly|InMemoryAssembly|) (assembly : Assembly) =
        if assembly.IsDynamic then DynamicAssembly
        else
            let location = assembly.Location
            if String.IsNullOrWhiteSpace location then InMemoryAssembly
            else StaticAssembly location

    type AssemblyName with
        /// Is signed assembly
        member an.IsStrongAssembly =
            let pkt = an.GetPublicKeyToken() 
            pkt <> null && pkt <> [||]

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