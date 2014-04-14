module internal Nessos.Vagrant.Utils

    open System
    open System.Reflection
    open System.Runtime.Serialization

    open Microsoft.FSharp.Control

    let runsMono = Type.GetType("Mono.Runtime") <> null

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
        let dict = new System.Collections.Generic.Dictionary<_,_>()
        fun x ->
            let found,y = dict.TryGetValue x
            if found then y
            else
                let y = f x
                dict.Add(x,y)
                y

    [<Literal>]
    let allBindings = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static

    type DynamicAssemblyState with
        member s.HasFreshTypes =
            let currentTypeCount =
                if not runsMono then
                    s.DynamicAssembly.GetTypes().Length
                else
                    // mono needs different approach since Assembly.GetTypes() only returns non-nested types
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
            | None -> failwith "not a DAG."
            | Some (t,_) ->
                let g0 = g |> List.choose (fun (t0, ts) -> if t0 = t then None else Some(t0, List.filter ((<>) t) ts))
                aux (t :: sorted) g0

        aux [] g

    /// A stateful agent implementation with readable inner state

    type StatefulAgent<'State, 'Input, 'Output>(init : 'State, f : 'State -> 'Input -> 'State * 'Output) =
        
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


    let mkStatefulAgent (init : 'S) (f : 'S -> 'I -> 'S * 'O) = new StatefulAgent<'S,'I,'O>(init, f)