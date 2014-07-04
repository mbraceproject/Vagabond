module internal Nessos.Vagrant.DependencyAnalysis

    open System
    open System.IO
    open System.Collections.Generic
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant.AssemblyParser
    open Nessos.Vagrant.SliceCompilerTypes

    open Microsoft.FSharp.Reflection

    let gatherObjectDependencies (graph:obj) =
        let types = new HashSet<Type> ()
        let assemblies = new HashSet<Assembly> ()

        let rec traverseType (t : Type) =
            if t = null then ()
            elif t.IsGenericType && not t.IsGenericTypeDefinition then
                types.Add(t.GetGenericTypeDefinition()) |> ignore
                for ga in t.GetGenericArguments() do
                    traverseType ga

            elif t.IsArray || t.IsPointer || t.IsByRef then
                traverseType <| t.GetElementType()
            else  
                types.Add t |> ignore

        let typeGatherer =
            {
                new IObjectVisitor with
                    member __.Visit<'T> (value : 'T) =
                        match box value with
                        | null -> ()
                        | :? Assembly as a -> assemblies.Add a |> ignore
                        | :? Type as t -> traverseType t
                        | o -> traverseType <| o.GetType()
            }

        do FsPickler.VisitObject(typeGatherer, graph)

        Seq.toArray types, Seq.toArray assemblies

    /// recursively traverse assembly dependency graph

    let traverseDependencies (state : DynamicAssemblyCompilerState option) (assemblies : seq<Assembly>) =

        let isIgnoredAssembly =
            let getPublicKey (a : Assembly) = a.GetName().GetPublicKey()
            let systemPkt = [| getPublicKey typeof<int>.Assembly ; getPublicKey typeof<int option>.Assembly |]
            let vagrantAssemblies = 
                [| 
                    typeof<Mono.Cecil.AssemblyDefinition>
                    typeof<Nessos.Vagrant.Cecil.IAssemblyParserConfig>
                    typeof<Nessos.Vagrant.AssemblyId>
                |] |> Array.map (fun t -> t.Assembly)

            fun (a:Assembly) ->
                Array.exists ((=) a) vagrantAssemblies ||
                    Array.exists ((=) (getPublicKey a)) systemPkt

        let tryResolveLoadedAssembly (an : AssemblyName) =
            match tryGetLoadedAssembly an.FullName with
            | Some a when isIgnoredAssembly a -> None
            | Some _ as s -> s
            | None ->
                match state |> Option.bind (fun s -> s.TryFindSliceInfo an.FullName) with
                // if a slice, return from compiler state directly as not loaded in AppDomain
                | Some (_,slice) -> Some slice.Assembly
                | None ->
                    try
                        // attempt loading from local machine
                        let a = Assembly.Load an
                        if isIgnoredAssembly a then None
                        else Some a
                    with :? FileNotFoundException -> None

        let rec traverseDependencyGraph (graph : Map<AssemblyId, Assembly * Assembly list>) (remaining : Assembly list) =
            match remaining with
            | [] -> graph |> Map.toList |> List.map snd
            | a :: tail when graph.ContainsKey a.AssemblyId || isIgnoredAssembly a -> traverseDependencyGraph graph tail
            | a :: tail -> 
                let dependencies = a.GetReferencedAssemblies() |> Array.choose tryResolveLoadedAssembly |> Array.toList
                traverseDependencyGraph (graph.Add(a.AssemblyId, (a, dependencies))) (dependencies @ tail)

        let dependencies =
            assemblies
            |> Seq.toList
            |> traverseDependencyGraph Map.empty
            |> getTopologicalOrdering

        // check for assemblies of identical qualified name
        match dependencies |> Seq.groupBy(fun a -> a.FullName) |> Seq.tryFind (fun (_,assemblies) -> Seq.length assemblies > 1) with
        | None -> ()
        | Some(name,_) -> 
            raise <| new VagrantException(sprintf "ran into duplicate assemblies of qualified name '%s'. This is not supported." name)

        dependencies


    /// parse a collection of assemblies, identify the dynamic assemblies that require slice compilation
    /// the dynamic assemblies are then parsed to Cecil and sorted topologically for correct compilation order.

    let parseDynamicAssemblies (state : DynamicAssemblyCompilerState) (assemblies : seq<Assembly>) =

        let isDynamicAssemblyRequiringCompilation (a : Assembly) =
            if a.IsDynamic then
                state.DynamicAssemblies.TryFind a.FullName 
                |> Option.forall (fun info -> info.HasFreshTypes)
            else
                false

        let rec traverse (graph : Map<AssemblyId, _>) (remaining : Assembly list) =
            match remaining with
            | [] -> graph
            | a :: rest when graph.ContainsKey a.AssemblyId || not <| isDynamicAssemblyRequiringCompilation a -> traverse graph rest
            | a :: rest ->
                // parse dynamic assembly
                let ((_,_,dependencies,_) as sliceData) = parseDynamicAssemblySlice state a

                let dependencies = dependencies |> List.choose tryGetLoadedAssembly

                let graph' = graph.Add(a.AssemblyId, (a, dependencies, sliceData))
                
                traverse graph' (rest @ dependencies)


        // topologically sort output
        let dynamicAssemblies = traverse Map.empty <| Seq.toList assemblies

        dynamicAssemblies
        |> Seq.map (function KeyValue(_, (a, deps,_)) -> a, deps |> List.filter (fun a -> a.IsDynamic))
        |> Seq.toList
        |> getTopologicalOrdering
        |> List.map (fun a -> let _,_,data = dynamicAssemblies.[a.AssemblyId] in data)



    type Dependencies = (Assembly * seq<Type>) list

    let computeDependencies (obj:obj) : Dependencies =
        let types, assemblies = gatherObjectDependencies obj
        
        types
        |> Seq.groupBy (fun t -> t.Assembly)
        |> Seq.append (assemblies |> Seq.map (fun a -> a, Seq.empty))
        |> Seq.toList

    /// determines the assemblies that require slice compilation based on given dependency input

    let getDynamicDependenciesRequiringCompilation (state : DynamicAssemblyCompilerState) (dependencies : Dependencies) =
        dependencies
        |> List.filter(fun (a,types) ->
            if a.IsDynamic then
                match state.DynamicAssemblies.TryFind a.FullName with
                | Some info -> types |> Seq.exists(fun t -> not <| info.TypeIndex.ContainsKey t.FullName)
                | None -> true
            else
                false)
        |> List.map fst


    /// reassigns assemblies so that the correct assembly slices are matched

    let remapDependencies (state : DynamicAssemblyCompilerState) (dependencies : Dependencies) =
        let remap (a : Assembly, ts : seq<Type>) =
            if a.IsDynamic then
                match state.DynamicAssemblies.TryFind a.FullName with
                | None -> raise <| new VagrantException(sprintf "no slices have been created for assembly '%s'." a.FullName)
                | Some info ->
                    let remapType (t : Type) =
                        match info.TypeIndex.TryFind t.FullName with
                        | None | Some (InNoSlice | InAllSlices) -> 
                            raise <| new VagrantException(sprintf "no slice corresponds to dynamic type '%O'." t)

                        | Some (InSpecificSlice slice) -> slice.Assembly

                    Seq.map remapType ts
            else Seq.singleton a

        dependencies |> Seq.collect remap |> traverseDependencies (Some state)