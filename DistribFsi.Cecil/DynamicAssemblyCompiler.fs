module Nessos.DistribFsi.FsiAssemblyCompiler

    open System
    open System.Reflection

    open Mono.Cecil
    open Mono.Collections.Generic
    open Mono.Reflection

    open Nessos.DistribFsi.TypeRefUpdater
    open Nessos.DistribFsi.TypeInitializationEraser

    let getCanonicalTypeName (t : TypeReference) =
        let rec aux carry (t : TypeReference) =
            if t = null then String.concat "+" carry
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
        member i.EmittedTypeCount = i.TypeIndex.Count

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


    let x = SerializationSupport.RegisterSerializer <| new FsPicklerSerializer()
    let touch () = x

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


    let compileDynamicAssemblySlice (state : DynamicAssemblyInfo) =

        let typeCount = state.Assembly.GetTypes().Length

        if typeCount = state.EmittedTypeCount then state 
        else
            let snapshot = AssemblySaver.Read state.Assembly

            // update snapshot to only contain slice
            let freshTypes = mkAssemblyDefinitionSlice state snapshot

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