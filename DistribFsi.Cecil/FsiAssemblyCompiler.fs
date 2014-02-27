module Nessos.DistribFsi.FsiAssemblyCompiler

    open System
    open System.Reflection
    open System.Text.RegularExpressions

    open Mono.Cecil

    open Nessos.DistribFsi.TypeRefUpdater


    type CompiledAssemblyState =
        {
            CompiledAssemblyCount : int

            FsiDynamicAssembly : Assembly
            TypeIndex : Map<string, Assembly>
        }
    with
        static member Empty =
            {
                CompiledAssemblyCount = 0
                FsiDynamicAssembly = AppDomain.CurrentDomain.GetAssemblies() |> Array.find(fun a -> a.IsDynamic && a.GetName().Name = "FSI-ASSEMBLY")
                TypeIndex = Map.empty
            }

    let tryFindFsiName =
        let fsiRegex = Regex("FSI_[0-9]{4}")
        fun (name : string) ->
            let m = fsiRegex.Match(name)
            if m.Success then
                let name = m.Groups.[0].Value
                Some name
            else
                None

    let rec collect (types : seq<TypeDefinition>) =
        seq {
            for t in types do
                yield t
                yield! collect t.NestedTypes
        } |> Seq.distinct

    module Map =
        let addMany (m : Map<'K,'V>) (kvs : ('K * 'V) seq) =
            Seq.fold (fun (m : Map<_,_>) (k,v) -> m.Add(k,v)) m kvs

    let mkReferenceUpdater (main : ModuleDefinition) (state : CompiledAssemblyState) =

        let rec tryResolveTypeImport (t : TypeReference) =
            match t with
            | null -> None
            | :? GenericInstanceType as gi ->
                let tyArgsRequireUpdate = ref false
                let gas = 
                    gi.GenericArguments 
                    |> Seq.map (fun ga -> 
                        match tryResolveTypeImport ga with 
                        | Some ga' -> tyArgsRequireUpdate := true ; ga'
                        | None -> ga)
                    |> Seq.toArray

                match tryResolveTypeImport <| gi.Resolve() with
                | None ->
                    if !tyArgsRequireUpdate then
                        gi.GenericArguments.Clear()
                        for ga in gas do gi.GenericArguments.Add(ga)
                        Some (gi :> TypeReference)
                    else
                        None
                | Some gt ->
                    let gi = new GenericInstanceType(gt)
                    for ga in gas do gi.GenericArguments.Add(ga)
                    Some (gi :> TypeReference)

            | t when t.Scope.Name = main.Name ->
                match t.FullName.Split('`').[0] |> tryFindFsiName with
                | None -> failwith "parse error!"
                | Some interactionId ->
                    match state.TypeIndex.TryFind interactionId with
                    | None -> None
                    | Some a ->
                        let rec loadType (t : TypeReference) =
                            match t.DeclaringType with
                            | null ->
                                let qname = t.FullName.Replace('/','.')
                                a.GetType(qname,true)
                            | dt ->
                                let dt0 = loadType dt
                                dt0.GetNestedType(t.Name)

                        let t0 = loadType t
                        let t00 = main.Import t0
                        Some t00
            | _ -> None

        { 
            new IReferenceUpdater with
                member __.UpdateTypeRef t = tryResolveTypeImport t
        }

    let compilePendingInteractions (state : CompiledAssemblyState) =
        let snapshot = Mono.Reflection.AssemblySaver.Read(state.FsiDynamicAssembly)
        let mainModule = snapshot.MainModule
        let types = mainModule.Types
        let compiled, newTypes = 
            types
            |> Seq.toList
            |> List.partition (fun td -> tryFindFsiName td.FullName |> Option.exists state.TypeIndex.ContainsKey)

        // remove already compiled types
        for t in compiled do types.Remove t |> ignore

        let compiledInteractions, currentInteraction = 
            let topLevelModuleNames = types |> Seq.choose (fun t -> tryFindFsiName t.Name) |> Set.ofSeq
            let currentInteraction = topLevelModuleNames |> Seq.max
            let compiledInteractions = topLevelModuleNames |> Set.remove currentInteraction |> Set.toList
            compiledInteractions, currentInteraction

        for t in newTypes do
            if t.FullName.Contains currentInteraction then types.Remove t |> ignore

        // remap type refs

        for t in collect types do
            updateTypeDefinition (mkReferenceUpdater mainModule state) t

        // compile

        let n = state.CompiledAssemblyCount + 1
        let target = sprintf "C:/mbrace/%d.dll" n

        mainModule.Assembly.Name.Name <- sprintf "FSI_%03d" n

        do snapshot.Write(target)

        let assembly = Assembly.ReflectionOnlyLoadFrom(target)

        let typeIndex = compiledInteractions |> Seq.map (fun name -> name, assembly) |> Map.addMany state.TypeIndex

        {
            CompiledAssemblyCount = n
            FsiDynamicAssembly = state.FsiDynamicAssembly
            TypeIndex = typeIndex
        }


