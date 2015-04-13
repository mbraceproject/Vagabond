module internal Nessos.Vagabond.DependencyAnalysis

open System
open System.IO
open System.Collections.Generic
open System.Reflection

open Nessos.FsPickler

open Nessos.Vagabond.AssemblyNaming
open Nessos.Vagabond.AssemblyParser
open Nessos.Vagabond.SliceCompilerTypes

open Microsoft.FSharp.Reflection

/// Assembly-specific topological ordering for assembly dependencies
let getAssemblyOrdering (dependencies : Graph<Assembly>) : Assembly list =
    match tryGetTopologicalOrdering dependencies with
    | Choice1Of2 sorted -> sorted
    | Choice2Of2 cycles ->
        // graph not DAG, return an appropriate exception
        let cycles = cycles |> Seq.map (fun (a,_) -> a.GetName().Name) |> String.concat ", "
        raise <| new VagabondException(sprintf "Came across cyclic dependencies of assemblies: %s" cycles)

/// returns all type depndencies for supplied object graph
let gatherObjectDependencies (graph:obj) : Type [] * Assembly [] =
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


/// assemblies ignored by Vagabond during assembly traversal
let private isIgnoredAssembly =
    let getPublicKey (a : Assembly) = a.GetName().GetPublicKey()
    let systemPkt = [| getPublicKey typeof<int>.Assembly |]
    let vagabondAssemblies = 
        [| 
            typeof<int option>
            typeof<Mono.Cecil.AssemblyDefinition>
            typeof<Nessos.Vagabond.AssemblyParser.IAssemblyParserConfig>
            typeof<Nessos.Vagabond.AssemblyId>
        |] |> Array.map (fun t -> t.Assembly)

    fun (a:Assembly) ->
        Array.exists ((=) a) vagabondAssemblies ||
            Array.exists ((=) (getPublicKey a)) systemPkt


/// locally resolve an assembly by qualified name
let private tryResolveAssembly ignoreF requireLoaded (state : DynamicAssemblyCompilerState option) (fullName : string) =
    match state |> Option.bind (fun s -> s.TryFindSliceInfo fullName) with
    | Some (_,info) -> Some info.Assembly
    | None ->
        match tryLoadAssembly fullName with
        | Some a when isIgnoredAssembly a || ignoreF a -> None
        | Some _ as r -> r
        | None when requireLoaded -> 
            let msg = sprintf "could not locate dependency '%s'. Try adding an explicit reference." fullName
            raise <| new VagabondException(msg)
        | None -> None


/// recursively traverse assembly dependency graph
let traverseDependencies ignoreF requireLoaded (state : DynamicAssemblyCompilerState option) (assemblies : seq<Assembly>) =

    let rec traverseDependencyGraph (graph : Map<AssemblyId, Assembly * Assembly list>) (remaining : Assembly list) =
        match remaining with
        | [] -> graph |> Map.toList |> List.map snd
        | a :: tail when graph.ContainsKey a.AssemblyId || isIgnoredAssembly a || ignoreF a -> traverseDependencyGraph graph tail
        | a :: tail -> 
            let dependencies = a.GetReferencedAssemblies() |> Array.choose (fun an -> tryResolveAssembly ignoreF requireLoaded state an.FullName) |> Array.toList
            traverseDependencyGraph (graph.Add(a.AssemblyId, (a, dependencies))) (dependencies @ tail)

    let dependencies =
        assemblies
        |> Seq.toList
        |> traverseDependencyGraph Map.empty
        |> getAssemblyOrdering

    // check for assemblies of identical qualified name
    match dependencies |> Seq.groupBy(fun a -> a.FullName) |> Seq.tryFind (fun (_,assemblies) -> Seq.length assemblies > 1) with
    | None -> ()
    | Some(name,_) -> 
        raise <| new VagabondException(sprintf "ran into duplicate assemblies of qualified name '%s'. This is not supported." name)

    dependencies


/// parse a collection of assemblies, identify the dynamic assemblies that require slice compilation
/// the dynamic assemblies are then parsed to Cecil and sorted topologically for correct compilation order.
let parseDynamicAssemblies ignoreF requireLoaded (state : DynamicAssemblyCompilerState) (assemblies : seq<Assembly>) =

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

            let dependencies = dependencies |> List.choose (tryResolveAssembly ignoreF requireLoaded (Some state))

            let graph' = graph.Add(a.AssemblyId, (a, dependencies, sliceData))
                
            traverse graph' (rest @ dependencies)


    // topologically sort output
    let dynamicAssemblies = traverse Map.empty <| Seq.toList assemblies

    dynamicAssemblies
    |> Seq.map (function KeyValue(_, (a, deps,_)) -> a, deps |> List.filter (fun a -> a.IsDynamic))
    |> Seq.toList
    |> getAssemblyOrdering
    |> List.map (fun a -> let _,_,data = dynamicAssemblies.[a.AssemblyId] in data)


/// Assembly dependency and its directly referenced types
type Dependencies = (Assembly * seq<Type>) list

/// compute dependencies for supplied object graph
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
let remapDependencies ignoreF requireLoaded (state : DynamicAssemblyCompilerState) (dependencies : Dependencies) =
    let remap (a : Assembly, ts : seq<Type>) =
        if a.IsDynamic then
            match state.DynamicAssemblies.TryFind a.FullName with
            | None -> raise <| new VagabondException(sprintf "no slices have been created for assembly '%s'." a.FullName)
            | Some info ->
                let remapType (t : Type) =
                    match info.TypeIndex.TryFind t.FullName with
                    | None | Some (InNoSlice | InAllSlices) -> 
                        raise <| new VagabondException(sprintf "no slice corresponds to dynamic type '%O'." t)

                    | Some (InSpecificSlice slice) -> slice.Assembly

                Seq.map remapType ts
        else Seq.singleton a

    dependencies |> Seq.collect remap |> traverseDependencies ignoreF requireLoaded (Some state)