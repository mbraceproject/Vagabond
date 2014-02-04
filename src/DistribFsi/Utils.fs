module internal Nessos.DistribFsi.Utils

    open System
    open System.Reflection
    open System.Collections.Generic

    open Nessos.DistribFsi

    let mkReadOnly (xs : 'T ref) = { new ReadOnly<'T> with member __.Value = xs.Value }

    type DeclarationIdUtils =
        static member Create(name : string, path : string) : DeclarationId =
            [ yield! path.Split('`').[0].Split([|'.';'+'|]) ; yield name ]

        static member OfMemberInfo(m : #MemberInfo) : DeclarationId =
            let dt = m.DeclaringType
            let dt = if dt.IsGenericType then dt.GetGenericTypeDefinition() else dt
            DeclarationIdUtils.Create(m.Name, dt.FullName)

    // eventually memoize
    let tryMemoize (f : 'T -> 'S option) =
        let cache = new Dictionary<'T, 'S> ()
        fun t ->
            let ok,s = cache.TryGetValue t
            if ok then Some s
            else
                match f t with
                | None -> None
                | Some s -> 
                    cache.Add(t,s)
                    Some s

    [<RequireQualifiedAccess>]
    module List =
        let partitioni (f : int -> 'a -> bool) (xs : 'a list) =
            let rec partitioner left right i rest =
                match rest with
                | [] -> (List.rev left, List.rev right)
                | x :: rest' ->
                    if f i x then
                        partitioner (x :: left) right (i+1) rest'
                    else
                        partitioner left (x :: right) (i+1) rest'

            partitioner [] [] 0 xs

        let tryMap (f : 'T -> 'S option) (ts : 'T list) : 'S list option =
            let rec gather acc rest =
                match rest with
                | [] -> Some <| List.rev acc
                | h :: t ->
                    match f h with
                    | Some s -> gather (s :: acc) t
                    | None -> None

            gather [] ts

    [<RequireQualifiedAccess>]
    module Guid =
        let toHex (g : Guid) =
            let bytes = g.ToByteArray()
            let builder = new System.Text.StringBuilder()
            for b in bytes do
                builder.AppendFormat("{0:x2}", b) |> ignore
            builder.ToString ()

    [<RequireQualifiedAccess>]
    module Set =
        let addMany (s : Set<'T>) (ts : 'T seq) =
            Seq.fold (fun (s : Set<_>) t -> s.Add t) s ts

        let (|Empty|Cons|) (s : Set<'T>) =
            if s.IsEmpty then Empty
            else
                let first = Seq.head s
                Cons(first, Set.remove first s)

    [<RequireQualifiedAccess>]
    module Map =
        let addMany (m : Map<'K,'V>) (kvs : ('K * 'V) seq) =
            Seq.fold (fun (m : Map<_,_>) (k,v) -> m.Add(k,v)) m kvs

    [<RequireQualifiedAccess>]
    module Option =
        let filter (f : 'T -> bool) (inp : 'T option) =
            match inp with Some t when f t -> inp | _ -> None

    [<RequireQualifiedAccess>]
    module Assembly =
        let tryFind =
            fun (name : string) ->
                AppDomain.CurrentDomain.GetAssemblies()
                |> Array.tryFind (fun a -> try a.FullName = name || a.GetName().Name = name with _ -> false)
            |> tryMemoize