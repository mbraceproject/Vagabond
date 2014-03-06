module Nessos.DistribFsi.FsiAssemblyCompiler

    open System
    open System.Reflection

    open Mono.Cecil
    open Mono.Reflection

    open Nessos.DistribFsi.TypeRefUpdater
    open Nessos.DistribFsi.TypeInitializationEraser

    let getCanonicalTypeName (t : TypeReference) =
        let rec aux carry (t : TypeReference) =
            if t = null then carry |> String.concat "+"
            else
                aux (t.Name :: carry) t.DeclaringType

        if String.IsNullOrEmpty t.Namespace then aux [] t
        else
            sprintf "%s.%s" t.Namespace <| aux [] t
            

    type DynamicAssemblyInfo =
        {
            Assembly : Assembly
            CompiledAssemblyCount : int
            TypeIndex : Map<string, Assembly>
        }
    with
        static member Init(a : Assembly) =
            {
                Assembly = a
                CompiledAssemblyCount = 0
                TypeIndex = Map.empty
            }


    let tryUpdateTypeReference (assembly : AssemblyDefinition) (state : DynamicAssemblyInfo) (t : TypeReference) =
        if t = null then None
        else
            let remoteAssembly =
                match t.Scope with
                | :? AssemblyNameReference as a when a.FullName <> assembly.FullName -> Some a.FullName
                | _ -> None

            if remoteAssembly.IsNone then
                let name = getCanonicalTypeName t
                match state.TypeIndex.TryFind name with
                | None -> None
                | Some a -> 
                    let rt = a.GetType(name, true)
                    let tI = assembly.MainModule.Import(rt)
                    Some tI
            else
                None


    do SerializationSupport.RegisterSerializer <| new FsPicklerSerializer()

    let touch () = ()

    module Map =
        let addMany (m : Map<'K,'V>) (kvs : ('K * 'V) seq) =
            Seq.fold (fun (m : Map<_,_>) (k,v) -> m.Add(k,v)) m kvs


    let compileDynamicAssemblySlice (state : DynamicAssemblyInfo) =

        let snapshot = AssemblySaver.Read state.Assembly

        let rec gatherFreshTypes (types : seq<TypeDefinition>) =
            seq {
                for t in types do
                    let name = getCanonicalTypeName t
                    if state.TypeIndex.ContainsKey name then
                        yield! gatherFreshTypes t.NestedTypes
                    else
                        yield t
            }

        let freshTypes = gatherFreshTypes snapshot.MainModule.Types |> Seq.toList

        if freshTypes.IsEmpty then state
        else
            // TODO : erase .cctors in non-fresh types
            do remapTypeReferences (tryUpdateTypeReference snapshot state) freshTypes

            let errors = eraseTypeInitializers state.Assembly snapshot

            // compile

            let n = state.CompiledAssemblyCount + 1
            let name = sprintf "FSI_%03d" n
            let target = sprintf "C:/mbrace/%s.dll" name
            
            do snapshot.Name.Name <- name
            do snapshot.Write(target)

            let assembly = Assembly.ReflectionOnlyLoadFrom(target)
            let typeIndex = freshTypes |> Seq.map (fun t -> getCanonicalTypeName t, assembly) |> Map.addMany state.TypeIndex

            { state with CompiledAssemblyCount = n ; TypeIndex = typeIndex }           
//        match t with
//        | null -> None
//        | t when t.Scope.Name = main.Name ->
//            match state.TypeIndex.TryFind <| t.FullName.Split('`').[0] with
//            | None -> None
//            | Some slice ->

                
//            match t.FullName.Split('`').[0] |> tryFindFsiName with
//            | None -> failwith "parse error!"
//            | Some interactionId ->
//                match state.TypeIndex.TryFind interactionId with
//                | None -> None
//                | Some a ->

//
//                    let t0 = loadType t
//                    let tI = main.Import t0
//                    Some tI
//        | _ -> None


        
//
//    open System
//    open System.Reflection
//    open System.Text.RegularExpressions
//
//    open Mono.Cecil
//
//    open Nessos.DistribFsi.TypeRefUpdater
//
//
//    type CompiledAssemblyState =
//        {
//            CompiledAssemblyCount : int
//
//            FsiDynamicAssembly : Assembly
//            TypeIndex : Map<string, Assembly>
//        }
//    with
//        static member Empty =
//            {
//                CompiledAssemblyCount = 0
//                FsiDynamicAssembly = AppDomain.CurrentDomain.GetAssemblies() |> Array.find(fun a -> a.IsDynamic && a.GetName().Name = "FSI-ASSEMBLY")
//                TypeIndex = Map.empty
//            }
//
//    let tryFindFsiName =
//        let fsiRegex = Regex("FSI_[0-9]{4}")
//        fun (name : string) ->
//            let m = fsiRegex.Match(name)
//            if m.Success then
//                let name = m.Groups.[0].Value
//                Some name
//            else
//                None
//
////    let rec collect (types : seq<TypeDefinition>) =
////        seq {
////            for t in types do
////                yield t
////                yield! collect t.NestedTypes
////        } |> Seq.distinct
//

//
//
//    
//    let touch () = ()
//

//
////        match t with
////        | null -> None
////        | :? GenericParameter as p ->
////            match p.DeclaringMethod with
////            | null -> 
////                tryResolveReferenceType main state p.DeclaringType 
////                |> Option.map (fun (dt : TypeReference) -> 
////                    let t = dt.GenericParameters |> Seq.find (fun p' -> p'.Name = p.Name)
////                    t :> TypeReference)
////            | m -> raise <| new NotImplementedException()
//////                tryResolveReferenceType main state m.DeclaringType
//////                |> Option.map (fun (dt : TypeReference) ->
//////                    
//////                    
//////                    )
////
////
////        | :? GenericInstanceType as gi ->
////            let tyArgsRequireUpdate = ref false
////            let gas = 
////                gi.GenericArguments 
////                |> Seq.map (fun ga -> 
////                    match tryResolveReferenceType main state ga with 
////                    | Some ga' -> tyArgsRequireUpdate := true ; ga'
////                    | None -> ga)
////                |> Seq.toArray
////
////            match tryResolveReferenceType main state <| gi.Resolve() with
////            | None ->
////                if !tyArgsRequireUpdate then
////                    gi.GenericArguments.Clear()
////                    for ga in gas do gi.GenericArguments.Add(ga)
////                    Some (gi :> TypeReference)
////                else
////                    None
////            | Some gt ->
////                let gi = new GenericInstanceType(gt)
////                for ga in gas do gi.GenericArguments.Add(ga)
////                Some (gi :> TypeReference)
////
////        | t when t.Scope.Name = main.Name ->
////            match t.FullName.Split('`').[0] |> tryFindFsiName with
////            | None -> failwith "parse error!"
////            | Some interactionId ->
////                match state.TypeIndex.TryFind interactionId with
////                | None -> None
////                | Some a ->
////                    let rec loadType (t : TypeReference) =
////                        match t.DeclaringType with
////                        | null ->
////                            let qname = t.FullName.Replace('/','.')
////                            a.GetType(qname,true)
////                        | dt ->
////                            let dt0 = loadType dt
////                            dt0.GetNestedType(t.Name)
////
////                    let t0 = loadType t
////                    let t00 = main.Import t0
////                    Some t00
////        | _ -> None
//
////        { 
////            new IReferenceUpdater with
////                member __.UpdateTypeRef t = tryResolveTypeImport t
////        }
//
//    let compilePendingInteractions (state : CompiledAssemblyState) =
//        let snapshot = Mono.Reflection.AssemblySaver.Read(state.FsiDynamicAssembly)
//        let mainModule = snapshot.MainModule
//        let types = mainModule.Types
//        let compiled, newTypes = 
//            types
//            |> Seq.toList
//            |> List.partition (fun td -> tryFindFsiName td.FullName |> Option.exists state.TypeIndex.ContainsKey)
//
//        // remove already compiled types
//        for t in compiled do types.Remove t |> ignore
//
//        let compiledInteractions, currentInteraction = 
//            let topLevelModuleNames = types |> Seq.choose (fun t -> tryFindFsiName t.Name) |> Set.ofSeq
//            let currentInteraction = topLevelModuleNames |> Seq.max
//            let compiledInteractions = topLevelModuleNames |> Set.remove currentInteraction |> Set.toList
//            compiledInteractions, currentInteraction
//
//        for t in newTypes do
//            if t.FullName.Contains currentInteraction then types.Remove t |> ignore
//
//        // remap type refs
//
//        do remapTypeReferences (tryUpdateReferenceType mainModule state) types
//

//
//        
//
//        // erase type initializers
//        let errors = Nessos.DistribFsi.TypeInitializationEraser.eraseTypeInitializers state.FsiDynamicAssembly snapshot
//
//        do snapshot.Write(target)
//
//        let assembly = Assembly.ReflectionOnlyLoadFrom(target)
//
//        let typeIndex = compiledInteractions |> Seq.map (fun name -> name, assembly) |> Map.addMany state.TypeIndex
//

//
