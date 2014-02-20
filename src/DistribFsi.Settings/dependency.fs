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
    let traverseAssemblyDependencies (assemblies : seq<Assembly>) =

        let ignoredAssemblies = 
            [| typeof<int> ; typeof<int option> |]
            |> Array.map (fun t -> t.Assembly)

        let isIgnoredAssembly a = Array.exists ((=) a) ignoredAssemblies

        let tryResolveAssembly (an : AssemblyName) =
            try Some <| Assembly.Load(an)
            with _ ->
                System.AppDomain.CurrentDomain.GetAssemblies() 
                |> Array.tryFind(fun a -> a.GetName() = an)

        let rec traverse (traversed : Map<string, Assembly>) (remaining : Assembly list) =
            match remaining with
            | [] -> traversed |> Map.toSeq |> Seq.map snd |> Seq.toList
            | a :: tail when traversed.ContainsKey a.Location || isIgnoredAssembly a -> traverse traversed tail
            | a :: tail -> 
                let newAssemblies = a.GetReferencedAssemblies() |> Seq.choose tryResolveAssembly |> Seq.toList
                traverse (traversed.Add(a.Location, a)) (newAssemblies @ tail)

        traverse Map.empty <| Seq.toList assemblies


    let computeObjectDependencies (state : InteractionCompilerInfo option) (obj:obj) =
        
        // get types in object
        let types = gatherTypesInObjectGraph obj

        let requiresCompilation = ref false
        let dependsOnCurrentInteraction = ref false
        let dependsOnInteraction = ref false

        // resolve assembly containing type
        // if in Fsi dynamic assembly, query interaction compiler
        // to determine statically compiled assemblies
        let tryGetDeclaringAssemblies (t : Type) =
            match state with
            | None -> [ t.Assembly ]
            | Some ici ->
                match ici.TryGetCompiledAssembly t with
                | None -> [ t.Assembly ]
                | Some result ->
                    dependsOnInteraction := true
                    match result with
                    | AlreadyCompiled a -> a.Assembly :: a.Dependencies
                    | RequiresCompilation(inCurrent,_) ->
                        dependsOnCurrentInteraction := inCurrent
                        requiresCompilation := true
                        []

        // use Seq.toList to force values in ref cells
        let assemblies = Seq.collect tryGetDeclaringAssemblies types |> Seq.distinct |> Seq.toList

        {
            Dependencies = if !requiresCompilation then None else Some <| traverseAssemblyDependencies assemblies

            RequiresAssemblyCompilation = requiresCompilation.Value
            DependsOnFsiCompiledAssembly = dependsOnInteraction.Value
            DependsOnCurrentInteraction = dependsOnCurrentInteraction.Value

        }