module internal MBrace.Vagabond.DependencyAnalysis

open System
open System.IO
open System.Collections.Generic
open System.Reflection

open MBrace.FsPickler

open MBrace.Vagabond.AssemblyNaming
open MBrace.Vagabond.AssemblyParser
open MBrace.Vagabond.SliceCompilerTypes

open Microsoft.FSharp.Reflection

[<AutoOpen>]
module private AssemblyInfo =
    let getPublicKey (a : Assembly) = a.GetName().GetPublicKey()
    let getFolder (a : Assembly) = Path.GetDirectoryName a.Location
    let systemPkt = getPublicKey typeof<int>.Assembly
    let coreLibFolder = getFolder typeof<int>.Assembly

    let vagabondAssemblies = 
        hset [|
            typeof<int option>.Assembly
            typeof<Mono.Cecil.AssemblyDefinition>.Assembly
            typeof<MBrace.Vagabond.AssemblyParser.IAssemblyParserConfig>.Assembly
            typeof<MBrace.Vagabond.AssemblyId>.Assembly
        |]

    /// assemblies ignored by Vagabond during assembly traversal
    let isIgnoredAssembly (a : Assembly) =
        vagabondAssemblies.Contains a ||
        systemPkt = getPublicKey a ||
        (not a.IsDynamic && coreLibFolder = getFolder a)

/// Assembly-specific topological ordering for assembly dependencies
let getAssemblyOrdering (dependencies : Graph<Assembly>) : Assembly list =
    // map assemblies to AssemblyId that has correct equality semantics
    let idGraph = dependencies |> Graph.map (fun a -> a.AssemblyId)
    let map = dependencies |> Seq.map (fun (a,_) -> a.AssemblyId, a) |> Map.ofSeq

    let rec aux (graph : Graph<AssemblyId>) =
        match Graph.tryGetTopologicalOrdering graph with
        | Choice1Of2 sorted -> sorted |> List.map (fun id -> map.[id])
        | Choice2Of2 cycle ->
#if NETSTANDARD
            // tolerate a certain set of microsoft-shipped assemblies that are cyclic
            match cycle |> List.tryFindIndex (fun id -> map.[id].GlobalAssemblyCache) with
            | Some i ->
                let n = List.length cycle
                // break cycle by trimming a single edge from the dependency graph
                let trimmedGraph = graph |> Graph.filterEdge (fun s e -> not (s = cycle.[i] && e = cycle.[i + 1 % n]))
                aux trimmedGraph
            | None ->
                // graph not DAG, return an appropriate exception
                let cycle = cycle |> Seq.map (fun id -> id.GetName().Name) |> String.concat ", "
                raise <| new VagabondException(sprintf "Found circular assembly dependencies: %s." cycle)
#else
            let cycle = cycle |> Seq.map (fun id -> id.GetName().Name) |> String.concat ", "
            raise <| new VagabondException(sprintf "Found circular assembly dependencies: %s." cycle)
#endif
    aux idGraph


/// locally resolve an assembly by qualified name
let private tryResolveAssembly (ignoreF : Assembly -> bool) (policy : AssemblyLookupPolicy) (state : AssemblyCompilerState option) (fullName : string) =
    match state |> Option.bind (fun s -> s.TryFindSliceInfo fullName) with
    | Some (_,info) -> Some info.Assembly
    | None ->
        let localAssembly =
            if AssemblyId.OfFullName(fullName).CanBeResolvedLocally policy then
                tryLoadAssembly fullName
            else
                tryGetLoadedAssembly fullName

        match localAssembly with
        | Some a when isIgnoredAssembly a || ignoreF a -> None
        | Some _ as r -> r
        | None when policy.HasFlag AssemblyLookupPolicy.RequireLocalDependenciesLoadedInAppDomain -> 
            let msg = sprintf "could not locate dependency '%s'. Try adding an explicit reference." fullName
            raise <| new VagabondException(msg)
        | None -> None


/// recursively traverse assembly dependency graph
let traverseDependencies ignoreF policy (state : AssemblyCompilerState option) (assemblies : seq<Assembly>) =

    let rec traverseDependencyGraph (graph : Map<AssemblyId, Assembly * Assembly list>) (remaining : Assembly list) =
        match remaining with
        | [] -> graph |> Seq.map (fun kv -> kv.Value) |> Seq.toList
        | a :: tail when graph.ContainsKey a.AssemblyId || isIgnoredAssembly a || ignoreF a -> traverseDependencyGraph graph tail
        | a :: tail ->
            // substitute reflection-only assemblies with their AppDomain loaded counterparts, if available
            let a =
                if a.ReflectionOnly then defaultArg (tryGetLoadedAssembly a.FullName) a
                else a

            let dependencies = a.GetReferencedAssemblies() |> Array.choose (fun an -> tryResolveAssembly ignoreF policy state an.FullName) |> Array.toList
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


/// Assembly dependency and its directly referenced types
type Dependency = Assembly * Type []

/// returns all type depndencies for supplied object graph
let gatherObjectDependencies ignoreF policy (state : AssemblyCompilerState option) (graph:obj) : Dependency list =
    let gathered = new Dictionary<Assembly, HashSet<Type>>()
    let addType (t : Type) =
        let ok,s = gathered.TryGetValue t.Assembly
        if ok then
            ignore <| s.Add t
        else
            let s = new HashSet<Type>([t])
            gathered.[t.Assembly] <- s

    let addAssembly (a : Assembly) =
        if not <| gathered.ContainsKey a then 
            gathered.[a] <- new HashSet<Type>()

    let containsType(t : Type) = 
        let ok,s = gathered.TryGetValue t.Assembly
        if ok then s.Contains t
        else false

    let rec traverseType (t : Type) =
        if t = null || containsType t then ()
        elif t.IsGenericType && not t.IsGenericTypeDefinition then
            addType (t.GetGenericTypeDefinition()) |> ignore
            for ga in t.GetGenericArguments() do
                traverseType ga

        elif t.IsArray || t.IsPointer || t.IsByRef then
            traverseType <| t.GetElementType()
        else  
            addType t |> ignore

    let typeGatherer =
        {
            new IObjectVisitor with
                member __.Visit<'T> (p : Pickler<'T>, value : 'T) =
                    if p.Kind > Kind.Primitive then
                        traverseType p.Type
                        if p.Kind > Kind.Value then
                            match box value with
                            | null -> ()
                            | :? Assembly as a -> addAssembly a
                            | :? Type as t -> traverseType t
                            | :? MemberInfo as m when m.DeclaringType <> null -> traverseType m.DeclaringType
                            | o -> traverseType <| o.GetType()

                    true
        }

    do FsPickler.VisitObject(typeGatherer, graph)
    
    let assemblies = gathered |> Seq.map (fun kv -> kv.Key)
    traverseDependencies ignoreF policy state assemblies
    |> Seq.map (fun a -> a, match gathered.TryFind a with Some ts -> Seq.toArray ts | None -> [||])
    |> Seq.toList

/// determines the assemblies that require slice compilation based on given dependency input
let getAssemblyDependenciesRequiringCompilation (state : AssemblyCompilerState) (dependencies : Dependency list) =
    dependencies
    |> Seq.filter(fun (a,types) ->
        match a with
        | StaticAssembly _ -> false
        | DynamicAssembly ->
            match state.DynamicAssemblies.TryFind a.FullName with
            | Some info -> types.Length = 0 || types |> Seq.exists(fun t -> not <| info.TypeIndex.ContainsKey t.FullName)
            | None -> true

        | InMemoryAssembly -> not <| state.InMemoryAssemblies.ContainsKey a.FullName)
    |> Seq.map fst
    |> Seq.toArray


/// reassigns assemblies so that the correct assembly slices are matched
let remapDependencies ignoreF policy (state : AssemblyCompilerState) (dependencies : Dependency list) =
    let remap (a : Assembly, ts : seq<Type>) =
        match a with
        | DynamicAssembly ->
            match state.DynamicAssemblies.TryFind a.FullName with
            | None -> raise <| new VagabondException(sprintf "no slices have been created for assembly '%s'." a.FullName)
            | Some info ->
                let remapType (t : Type) =
                    match info.TypeIndex.TryFind t.FullName with
                    | None | Some (InNoSlice | InAllSlices) -> 
                        raise <| new VagabondException(sprintf "no slice corresponds to dynamic type '%O'." t)

                    | Some (InSpecificSlice slice) -> slice.Assembly

                Seq.map remapType ts

        | InMemoryAssembly ->
            match state.InMemoryAssemblies.TryFind a.FullName with
            | None -> raise <| new VagabondException(sprintf "no compilation has been created for assembly '%s'." a.FullName)
            | Some info -> Seq.singleton info.CompiledAssembly

        | StaticAssembly _ -> Seq.singleton a

    dependencies |> Seq.collect remap |> traverseDependencies ignoreF policy (Some state)