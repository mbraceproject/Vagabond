module internal Nessos.DistribFsi.DependencyAnalysis

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Runtime.Serialization
    open System.Reflection

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.ExprShape


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
    


    /// gathers all types that occur in an object graph

    let gatherTypesInObjectGraph (obj:obj) =
        let gathered = new HashSet<Type>()
        let objCounter = new ObjectIDGenerator()
        let inline add t = gathered.Add t |> ignore
        
        let rec traverseObj (obj : obj) =
            match obj with
            | null -> ()
            | :? Type as t -> traverseType t
            | :? MemberInfo as m -> traverseType m.DeclaringType
            | _ ->
                let _,firstTime = objCounter.GetId obj
                if firstTime then
                    let t = obj.GetType()
                    traverseType t

                    if t.IsPrimitive || t = typeof<string> then ()
                    elif t.IsArray then
                        let et = t.GetElementType()
                        if et.IsPrimitive then ()
                        else
                            for e in obj :?> Array do
                                traverseObj e
                    else
                        let fields = t.GetFields(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)

                        for f in fields |> Array.filter (fun f -> not f.FieldType.IsPrimitive) do
                            let o = f.GetValue(obj)
                            traverseObj o

        and traverseType (t : Type) =   
            if t.IsArray || t.IsByRef || t.IsPointer then
                traverseType <| t.GetElementType()
            elif t.IsGenericType && not t.IsGenericTypeDefinition then
                add <| t.GetGenericTypeDefinition()
                for ta in t.GetGenericArguments() do
                    traverseType ta
            else
                add t

        do traverseObj obj
        gathered |> Seq.toList


    /// matches quotations of form <@ symbol @>
    let (|SymbolExpr|_|) (expr : Expr) =
        let rec peel expr =
            match expr with
            | Lambda(_,body) -> peel body
            | Call(_,meth,_) -> meth :> MemberInfo |> Some
            | PropertyGet(_,prop,_) -> prop :> MemberInfo |> Some
            | _ -> None

        peel expr

    /// gathers all memberInfo's that occur in quotation
    let gatherMembersOfExpr (expr : Expr) =
        let gathered = new HashSet<MemberInfo> ()
        let inline add x = gathered.Add x |> ignore
        let rec aux (e : Expr) =
            try add e.Type with _ -> ()
            match e with
            | ShapeVar _ -> ()
            | ShapeLambda(_, body) -> aux body
            | ShapeCombination(_,exprs) -> 
                match e with
                | Call(_,m,_) -> add m
                | PropertyGet(_,p,_) -> add p
                | _ -> ()

                for e in exprs do aux e

        do aux expr
        gathered |> Seq.toList

    // recursively traverse assembly dependency graph
    let traverseDependencies (assemblies : seq<Assembly>) =

        let ignoredAssemblies = 
            [| typeof<int> ; typeof<int option> |]
            |> Array.map (fun t -> t.Assembly)

        let isIgnoredAssembly a = Array.exists ((=) a) ignoredAssemblies

        let tryResolveLoadedAssembly (an : AssemblyName) =
            System.AppDomain.CurrentDomain.GetAssemblies() |> Array.tryFind (fun a -> a.GetName() = an)

        let rec traverseDependencyGraph (graph : Map<string, Assembly * Assembly list>) (remaining : Assembly list) =
            match remaining with
            | [] -> graph
            | a :: tail when graph.ContainsKey a.FullName || isIgnoredAssembly a -> traverseDependencyGraph graph tail
            | a :: tail -> 
                let dependencies = a.GetReferencedAssemblies() |> Array.choose tryResolveLoadedAssembly |> Array.toList
                traverseDependencyGraph (graph.Add(a.FullName, (a, dependencies))) (dependencies @ tail)

        assemblies
        |> Seq.toList
        |> traverseDependencyGraph Map.empty
        |> Map.toList
        |> List.map snd
        |> getTopologicalOrdering