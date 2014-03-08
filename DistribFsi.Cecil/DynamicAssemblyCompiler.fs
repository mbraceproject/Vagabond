module Nessos.DistribFsi.FsiAssemblyCompiler

    open System
    open FsPickler
    open System.Reflection

    open Mono.Cecil
    open Mono.Collections.Generic
    open Mono.Reflection

    open Nessos.DistribFsi.DependencyAnalysis
    open Nessos.DistribFsi.TypeRefUpdater
    open Nessos.DistribFsi.TypeInitializationEraser


//    with
//        static member Init (path : string) =
//            if not <| System.IO.Directory.Exists(path) then failwith "invalid directory"
//            {
//                ClientId = Guid.NewGuid().ToString()
//                OutputDirectory = path
//                DynamicAssemblies = Map.empty
//            }


    let getCanonicalTypeName (t : TypeReference) =
        let rec aux carry (t : TypeReference) =
            if t = null then String.concat "+" carry
            else
                aux (t.Name :: carry) t.DeclaringType

        if String.IsNullOrEmpty t.Namespace then aux [] t
        else
            sprintf "%s.%s" t.Namespace <| aux [] t


    let tryUpdateTypeReference (assembly : AssemblyDefinition) (state : GlobalDynamicAssemblyState) (t : TypeReference) =
        if t = null then None
        else
            let assemblyName =
                match t.Scope with
                | :? AssemblyNameReference as a -> a.FullName
                | _ -> assembly.FullName

            match state.DynamicAssemblies.TryFind assemblyName with
            | None -> None
            | Some info ->
                let name = getCanonicalTypeName t
                match info.TypeIndex.TryFind name with
                | None -> None
                | Some a ->
                    let rt = a.GetType(name, true)
                    let tI = assembly.MainModule.Import(rt)
                    Some tI

    module Map =
        let addMany (m : Map<'K,'V>) (kvs : ('K * 'V) seq) =
            Seq.fold (fun (m : Map<_,_>) (k,v) -> m.Add(k,v)) m kvs

    //
    //  traverse the type hierarchy to: 
    //  1) identify and collect all freshly emitted type definitions
    //  2) for all the previously emitted types:
    //      a) erase type definitions that do not have new nested types
    //      b) keep all others for scaffolding
    //

    let mkAssemblyDefinitionSlice (state : DynamicAssemblyInfo) (assemblyDef : AssemblyDefinition) =

        let eraseTypeContents(t : TypeDefinition) =
            t.Methods.Clear() ; t.Fields.Clear()
            t.Events.Clear() ; t.Properties.Clear()
            t.CustomAttributes.Clear()
            for gt in t.GenericParameters do
                gt.CustomAttributes.Clear()
                gt.Constraints.Clear()

        let rec gatherFreshTypes (types : Collection<TypeDefinition>) =
            seq {
                let erasedTypes = ref []

                for t in types do
                    let name = getCanonicalTypeName t
                    
                    if name = "<Module>" then ()
                    elif state.TypeIndex.ContainsKey name then
                        // type has already been emitted, is to be kept only in event of new nested subtypes
                        let freshNested = gatherFreshTypes t.NestedTypes |> Seq.toArray

                        if freshNested.Length = 0 then
                            // no new nested subtypes, schedule for erasure
                            erasedTypes := t :: !erasedTypes
                        else
                            // has nested subtypes, keep type for scaffolding but erase contents
                            yield! freshNested
                            do eraseTypeContents t
                    else
                        // new type, collect the entire hierarchy
                        yield! collectHierarchy t

                // Mono.Collection cannot be updated while enumerated, perform erasure now
                for t in !erasedTypes do 
                    types.Remove(t) |> ignore
            }

        and collectHierarchy (t : TypeDefinition) =
            seq {
                yield t
                for nt in t.NestedTypes do
                    yield! collectHierarchy nt
            }

        gatherFreshTypes assemblyDef.MainModule.Types |> Seq.toArray


    let compileDynamicAssemblySlice (state : GlobalDynamicAssemblyState) (assembly : Assembly) =
        
        if not assembly.IsDynamic then invalidArg assembly.FullName "not a dynamic assembly."

        let assemblyInfo =
            match state.DynamicAssemblies.TryFind assembly.FullName with
            | None -> DynamicAssemblyInfo.Init assembly
            | Some info -> info

        if not assemblyInfo.HasFreshTypes then state
        else
            // parse dynamic assembly
            let snapshot = AssemblySaver.Read assemblyInfo.Assembly

            // update snapshot to only contain current slice
            let freshTypes = mkAssemblyDefinitionSlice assemblyInfo snapshot

            // TODO : erase .cctors in non-fresh types
            do remapTypeReferences (tryUpdateTypeReference snapshot state) freshTypes

            let errors = eraseTypeInitializers assemblyInfo.Assembly snapshot

            // compile

            let n = assemblyInfo.CompiledAssemblies.Length + 1
            let name = sprintf "%s_%s_%03d" assemblyInfo.Name.Name state.ClientId n
            let target = System.IO.Path.Combine(state.OutputDirectory, name + ".dll")

            do snapshot.Name.Name <- name
            do snapshot.Write(target)

            // update type index & compiled assembly info
            let assembly = Assembly.ReflectionOnlyLoadFrom(target)
            let compiledAssemblies = assembly :: assemblyInfo.CompiledAssemblies
            let typeIndex = freshTypes |> Seq.map (fun t -> getCanonicalTypeName t, assembly) |> Map.addMany assemblyInfo.TypeIndex

            let assemblyInfo = { assemblyInfo with CompiledAssemblies = compiledAssemblies ; TypeIndex = typeIndex }

            { state with DynamicAssemblies = state.DynamicAssemblies.Add(assemblyInfo.Assembly.FullName, assemblyInfo)}


//    let private testState = 
//        do SerializationSupport.RegisterSerializer <| new FsPicklerSerializer()
//        ref <| GlobalDynamicAssemblyState.Init @"C:\mbrace\"
//
//    let getObjectDependencies (obj:obj) =
//        let types = gatherTypesInObjectGraph obj
//        let assemblyInfo = types |> Seq.groupBy (fun t -> t.Assembly) |> Seq.toList
//
//        let excludedDynamicAssemblies = 
//            assemblyInfo 
//            |> Seq.choose (fun (a, types) -> 
//                match testState.Value.DynamicAssemblies.TryFind a.FullName with
//                | None -> None
//                | Some info -> 
//                    let doesNotRequireCompilation = types |> Seq.forall(fun t -> info.TypeIndex.ContainsKey t.FullName)
//                    if doesNotRequireCompilation then Some a.FullName
//                    else None)
//            |> set
//
//        let allAssemblies = assemblyInfo |> List.map fst |> traverseDependencies
//
//        [
//            for a in allAssemblies do
//                if a.IsDynamic then 
//                    if not <| excludedDynamicAssemblies.Contains a.FullName then
//                        testState := compileDynamicAssemblySlice !testState a
//                    yield! testState.Value.DynamicAssemblies.[a.FullName].CompiledAssemblies
//                else 
//                    yield a
//        ]
//
