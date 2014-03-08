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

    open Nessos.DistribFsi.FsiAssemblyCompiler

    open FsPickler


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

    module Choice2 =
        let partition (inputs : Choice<'T,'S> list) =
            let rec aux p1 p2 rest =
                match rest with
                | Choice1Of2 t :: tl -> aux (t :: p1) p2 tl
                | Choice2Of2 s :: tl -> aux p1 (s :: p2) tl
                | [] -> List.rev p1, List.rev p2

            aux [] [] inputs
    


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


    //
    //
    //

    let getExportableDynamicAssemblyInfo (pickler : FsPickler) (info : DynamicAssemblyInfo) =
        let fieldPickles, pickleFailures =
            info.StaticFields
            |> List.map(fun fI -> 
                try
                    let value = fI.GetValue(null)
                    let payload = (fI, value)
                    let pickle = pickler.Pickle<FieldInfo * obj>(payload)
                    Choice1Of2 (fI.ToString(), pickle)
                with e -> Choice2Of2 (e, fI))
            |> Choice2.partition

        let exportable = { ActualName = info.Assembly.FullName ; Slices = info.CompiledAssemblies ; ValueInitializationBlobs = fieldPickles }

        exportable, pickleFailures

    let loadExportedDynamicAssemblyInfo (pickler : FsPickler) (info : ExportedDynamicAssemblyInfo) =
        // load value initialization blobs
        for (_,blob) in info.ValueInitializationBlobs do
            let field,value = pickler.UnPickle<FieldInfo * obj>(blob)
            field.SetValue(null, value)  

    let getPortableDependencyInfo (pickler : FsPickler) (assemblies : Assembly list) (info : DynamicAssemblyInfo list) =
        let info, failures = info |> List.map (getExportableDynamicAssemblyInfo pickler) |> List.unzip
        let failures = List.concat failures
        let depInfo = {
            AllDependencies = assemblies
            DynamicAssemblies = info
        }

        depInfo, failures


    let loadPortableDependencyInfo (pickler : FsPickler) (info : PortableDependencyInfo) =
        info.DynamicAssemblies |> List.iter (loadExportedDynamicAssemblyInfo pickler)



    let computeObjectDependencies (state : GlobalDynamicAssemblyState) (obj:obj) =

        let types = gatherTypesInObjectGraph obj
        let assemblyInfo = types |> Seq.groupBy (fun t -> t.Assembly) |> Seq.toList

        let allAssemblies = assemblyInfo |> List.map fst |> traverseDependencies
        let topLevelDynamicAssemblies =
            assemblyInfo 
            |> Seq.map (fun (a, types) -> 
                let requiresCompilation =
                    match state.DynamicAssemblies.TryFind a.FullName with
                    | None -> true // always attempt to compile dynamic assemblies whose types do not directly appear in the object graph
                    | Some info -> types |> Seq.forall (fun t -> info.TypeIndex.ContainsKey t.FullName) |> not

                a.FullName, requiresCompilation)
            |> Map.ofSeq

        let isDynamicAssemblyRequiringCompilation (a : Assembly) =
            match topLevelDynamicAssemblies.TryFind a.FullName with
            | None -> true
            | Some requires -> requires

        // traverse and compile
        let state = ref state
        let dynamicAssemblies = ref []

        let exportedAssemblies =
            [
                for a in allAssemblies do
                    if a.IsDynamic then 
                        if isDynamicAssemblyRequiringCompilation a then
                            state := compileDynamicAssemblySlice !state a

                        let assemblyInfo = state.Value.DynamicAssemblies.[a.FullName]

                        dynamicAssemblies := assemblyInfo :: !dynamicAssemblies
                        yield! assemblyInfo.CompiledAssemblies

                    else 
                        yield a
            ]

        exportedAssemblies, !dynamicAssemblies, !state


//    let computeObjectPickler (pickler : FsPickler) (state : GlobalDynamicAssemblyState) (obj:obj) =
//        let info, errors, state = computeObjectDependencies pickler state obj
//
//        let objPickle = pickler.Pickle<obj> obj
//
//        let pickle =
//            {
//                Pickle = objPickle
//                DependencyInfo = info
//            }
//
//        pickle, errors, state
//
//
//    let loadObjectPickle (pickler : FsPickler) (pickle : Pickle) =
//        do loadPortableDependencyInfo pickler pickle.DependencyInfo
//
//        pickler.UnPickle<obj> pickle.Pickle
