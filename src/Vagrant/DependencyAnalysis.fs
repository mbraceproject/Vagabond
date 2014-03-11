module internal Nessos.Vagrant.DependencyAnalysis

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Runtime.Serialization
    open System.Reflection

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.ExprShape

    open Nessos.Vagrant.Utils

    open FsPickler

    /// gathers all types that occur in an object graph

    let gatherTypesInObjectGraph (obj:obj) =
        let gathered = new HashSet<Type>()
        let tracker = new ObjectTracker()
        let inline add t = gathered.Add t |> ignore
        
        let rec traverseObj (obj : obj) =
            match obj with
            | null -> ()
            | :? Type as t -> traverseType t
            | :? MemberInfo as m -> traverseType m.DeclaringType
            | _ ->
                if tracker.IsFirstOccurence obj then
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


//    /// matches quotations of form <@ symbol @>
//    let (|SymbolExpr|_|) (expr : Expr) =
//        let rec peel expr =
//            match expr with
//            | Lambda(_,body) -> peel body
//            | Call(_,meth,_) -> meth :> MemberInfo |> Some
//            | PropertyGet(_,prop,_) -> prop :> MemberInfo |> Some
//            | _ -> None
//
//        peel expr
//
//    /// gathers all memberInfo's that occur in quotation
//    let gatherMembersOfExpr (expr : Expr) =
//        let gathered = new HashSet<MemberInfo> ()
//        let inline add x = gathered.Add x |> ignore
//        let rec aux (e : Expr) =
//            try add e.Type with _ -> ()
//            match e with
//            | ShapeVar _ -> ()
//            | ShapeLambda(_, body) -> aux body
//            | ShapeCombination(_,exprs) -> 
//                match e with
//                | Call(_,m,_) -> add m
//                | PropertyGet(_,p,_) -> add p
//                | _ -> ()
//
//                for e in exprs do aux e
//
//        do aux expr
//        gathered |> Seq.toList

    // recursively traverse assembly dependency graph
    let traverseDependencies (assemblies : seq<Assembly>) =

        let ignoredAssemblies = 
            [| typeof<int> ; typeof<int option> |]
            |> Array.map (fun t -> t.Assembly)

        let isIgnoredAssembly a = Array.exists ((=) a) ignoredAssemblies

        let tryResolveLoadedAssembly (an : AssemblyName) =
            System.AppDomain.CurrentDomain.GetAssemblies() 
            |> Array.tryFind (fun a -> a.FullName = an.FullName)
            |> Option.filter (not << isIgnoredAssembly)

        let rec traverseDependencyGraph (graph : Map<string, Assembly * Assembly list>) (remaining : Assembly list) =
            match remaining with
            | [] -> graph |> Map.toList |> List.map snd
            | a :: tail when graph.ContainsKey a.FullName || isIgnoredAssembly a -> traverseDependencyGraph graph tail
            | a :: tail -> 
                let dependencies = a.GetReferencedAssemblies() |> Array.choose tryResolveLoadedAssembly |> Array.toList
                traverseDependencyGraph (graph.Add(a.FullName, (a, dependencies))) (dependencies @ tail)

        assemblies
        |> Seq.toList
        |> traverseDependencyGraph Map.empty
        |> getTopologicalOrdering



    let computeObjectDependencies (state : GlobalDynamicAssemblyState) (obj : obj) =

        let types = gatherTypesInObjectGraph obj
        let assemblyInfo = types |> Seq.groupBy (fun t -> t.Assembly) |> Seq.toList

        let allAssemblies = assemblyInfo |> Seq.map fst |> traverseDependencies

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
            if a.IsDynamic then
                match topLevelDynamicAssemblies.TryFind a.FullName with
                | None -> true
                | Some requires -> requires
            else
                false

        let slices = 
            allAssemblies 
            |> List.filter isDynamicAssemblyRequiringCompilation 
            |> List.choose compiler.TryCompileNextSlice

        let exportedAssemblies =
            [
                for a in allAssemblies do
                    match compiler.State.DynamicAssemblies.TryFind a.FullName with
                    | None -> yield a
                    | Some d -> yield! d.GeneratedSlices |> Seq.map (fun s -> s.Assembly)
            ]

        slices, exportedAssemblies


    //
    //
    //

//    let getDynamicAssemblySlice (compiler : SliceCompilationServer) (a : Assembly) =
//        assert a.IsDynamic
//        
//        traverseDependencies [a] 
//        |> List.filter (fun a -> a.IsDynamic)
//        |> List.choose compiler.TryCompileNextSlice
//
//
//

            




//        let newSliceInfo = ref []
//
//        let exportedDependencies =
//            [
//                for a in allAssemblies do
//                    if a.IsDynamic then 
//                        if isDynamicAssemblyRequiringCompilation a then
//                            let slice = compiler.TryCompileNextSlice(a).Value
//                            newSliceInfo := slice :: !newSliceInfo
//
//                        let assemblyInfo = compiler.State.DynamicAssemblies.[a.FullName]
//                        yield! assemblyInfo.CompiledAssemblies
//
//                    else 
//                        yield a
//            ]
//
//        exported
        


//        if not a.IsDynamic then invalidArg a.FullName "not a dynamic assembly."
//
//        let dependencies = 

//    let getExportableDynamicAssemblyInfo (pickler : FsPickler) (info : DynamicAssemblyInfo) =
//        let fieldPickles, pickleFailures =
//            info.StaticFields
//            |> List.map(fun fI -> 
//                try
//                    let value = fI.GetValue(null)
//                    let payload = (fI, value)
//                    let pickle = pickler.Pickle<FieldInfo * obj>(payload)
//                    Choice1Of2 (fI.ToString(), pickle)
//                with e -> Choice2Of2 (e, fI))
//            |> Choice2.partition
//
//        let exportable = { ActualName = info.Assembly.FullName ; Slices = info.CompiledAssemblies ; ValueInitializationBlobs = fieldPickles }
//
//        exportable, pickleFailures
//

//
//    let getPortableDependencyInfo (pickler : FsPickler) (sourceId : string) (assemblies : Assembly list) (info : DynamicAssemblyInfo list) =
//        let info, failures = info |> List.map (getExportableDynamicAssemblyInfo pickler) |> List.unzip
//        let failures = List.concat failures
//        let depInfo = {
//            SourceId = sourceId
//            AllDependencies = assemblies
//            DynamicAssemblies = info
//        }
//
//        depInfo, failures
//
//
//    let loadPortableDependencyInfo (pickler : FsPickler) (info : PortableDependencyInfo) =
//        info.DynamicAssemblies |> List.iter (loadExportedDynamicAssemblyInfo pickler)
//
//
//
//    let computeObjectDependencies (state : GlobalDynamicAssemblyState) (obj:obj) =
//


//
//        // traverse and compile
//        let state = ref state

//
//        exportedAssemblies, !dynamicAssemblies, !state
//
//
