module internal Nessos.Vagrant.Utils

    open Mono.Cecil
    open Mono.Collections.Generic

    open System
    open System.Reflection


    module Map =
        let addMany (m : Map<'K,'V>) (kvs : ('K * 'V) seq) =
            Seq.fold (fun (m : Map<_,_>) (k,v) -> m.Add(k,v)) m kvs

    module Choice2 =
        let partition (inputs : Choice<'T,'S> list) =
            let rec aux p1 p2 rest =
                match rest with
                | Choice1Of2 t :: tl -> aux (t :: p1) p2 tl
                | Choice2Of2 s :: tl -> aux p1 (s :: p2) tl
                | [] -> List.rev p1, List.rev p2

            aux [] [] inputs

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

    type ObjectTracker() =
        let objectCounter = new System.Runtime.Serialization.ObjectIDGenerator()

        member __.IsFirstOccurence<'T when 'T : not struct>(x : 'T) =
            if obj.ReferenceEquals(x,null) then false
            else
                let _,firstTime = objectCounter.GetId x in firstTime


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


    // Mono.Cecil utilities

    module Collection =
        let update (f : 'T -> 'T) (collection : Collection<'T>) : unit =
            let updated = collection |> Seq.map f |> Seq.toArray
            collection.Clear()
            for t in updated do collection.Add(t)

        let map (f : 'T -> 'U) (tcollection : Collection<'T>) : Collection<'U> =
            let ucollection = new Collection<'U>()
            for t in tcollection do ucollection.Add(f t)
            ucollection


    type TypeReference with
        member t.CanonicalName =
            let rec aux carry (t : TypeReference) =
                if t = null then String.concat "+" carry
                else
                    aux (t.Name :: carry) t.DeclaringType

            if String.IsNullOrEmpty t.Namespace then aux [] t
            else
                sprintf "%s.%s" t.Namespace <| aux [] t


        member t.ContainingAssembly =
            match t.Scope with
            | :? AssemblyNameReference as a -> Some a.FullName
//            | :? ModuleReference as m -> m.
            | _ -> None