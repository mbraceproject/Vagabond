module internal Nessos.DistribFsi.Shell.Utils

    open System
    open System.Reflection
    open System.Collections.Generic

    open Nessos.DistribFsi

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

    [<RequireQualifiedAccess>]
    module Map =
        let addMany (m : Map<'K,'V>) (kvs : ('K * 'V) seq) =
            Seq.fold (fun (m : Map<_,_>) (k,v) -> m.Add(k,v)) m kvs

    [<RequireQualifiedAccess>]
    module Option =
        let filter (f : 'T -> bool) (inp : 'T option) =
            match inp with Some t when f t -> inp | _ -> None


    let allFlags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static

    let rec containsReflectedDefinitionAttribute (m : MemberInfo) =
        let isReflectedDefnAttr (o:obj) = o <> null && o.GetType() = typeof<ReflectedDefinitionAttribute>
        let hasAttr = m.GetCustomAttributes(true) |> Array.exists isReflectedDefnAttr
        if hasAttr then true
        elif m.DeclaringType = null then false
        else
            containsReflectedDefinitionAttribute m.DeclaringType

    let declarationDefinesClosures (declaringType : Type) (name : string) =
        let rec getNestedTypes(t : Type) =
            seq {
                for nt in t.GetNestedTypes(BindingFlags.NonPublic ||| BindingFlags.Public) do
                    yield nt
                    yield! getNestedTypes nt
            }

        let isClosureType (t : Type) = t.Name.StartsWith(name + "@") //|| t.Name.StartsWith("clo@")

        declaringType
        |> getNestedTypes
        |> Seq.exists isClosureType