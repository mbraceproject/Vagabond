module internal Nessos.Vagrant.SliceCompiler

    open System
    open System.IO
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open System.Reflection

    open Mono.Cecil
    open Mono.Collections.Generic
    open Mono.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.Serialization
    open Nessos.Vagrant.AssemblyParser
    open Nessos.Vagrant.DependencyAnalysis

    /// create an initial, empty compiler state

    let initCompilerState (profiles : IDynamicAssemblyProfile list) (outDirectory : string) =
        let uuid = Guid.NewGuid()
        let mkSliceName (name : string) (id : int) = sprintf "%s_%O_%d" name uuid id
        let assemblyRegex = Regex(sprintf "^(.*)_%O_([0-9]+)" uuid)
        let tryExtractDynamicAssemblyId (assemblyName : string) =
            let m = assemblyRegex.Match(assemblyName)
            if m.Success then 
                let dynamicName = m.Groups.[1].Value
                let sliceId = int <| m.Groups.[2].Value
                Some (dynamicName, sliceId)
            else
                None
        {
            ServerId = Guid.NewGuid()
            OutputDirectory = outDirectory
            DynamicAssemblies = Map.empty
            Profiles = profiles

            TryGetDynamicAssemblyId = tryExtractDynamicAssemblyId
            CreateAssemblySliceName = mkSliceName
        }





    //
    //  TODO: for performance reasons, it would be a good idea to merge Mono.Reflection's AssemblySaver and the slicer
    //  The *entire* dynamic assembly will be parsed every time a slice is requested.
    //
    //  traverse the type hierarchy to: 
    //  1) identify and collect all freshly emitted type definitions
    //  2) for all the previously emitted types:
    //      a) erase type definitions that do not have new nested types
    //      b) keep all others for scaffolding
    //




//    let mkAssemblyDefinitionSlice (state : DynamicAssemblyState) (assemblyDef : AssemblyDefinition) =
//
//        let gathered = HashSet<TypeDefinition * Type> ()
//
//        let eraseTypeContents(t : TypeDefinition) =
//            t.Methods.Clear() ; t.Fields.Clear()
//            t.Events.Clear() ; t.Properties.Clear()
//            t.CustomAttributes.Clear()
//            for gt in t.GenericParameters do
//                gt.CustomAttributes.Clear()
//                gt.Constraints.Clear()
//
//        let rec traverseTypeDefinitions (types : Collection<TypeDefinition>) =
//            seq {
//                for t in Seq.toArray types do
//                    match t.CanonicalName with
//                    | "<Module>" -> ()
//                    | name ->
//                        // resolve reflected type from dynamic assembly
//                        let reflectedType = state.DynamicAssembly.GetType(name, true)
//                    
//                        if state.Profile.AlwaysIncludeType reflectedType then ()
//                        elif state.Profile.EraseType reflectedType then types.Remove(t) |> ignore
//
//                        elif state.TypeIndex.ContainsKey name then
//                            // type has already been emitted, is to be kept only in event of new nested subtypes
//                            let freshNested = traverseTypeDefinitions t.NestedTypes |> Seq.toArray
//
//                            if freshNested.Length = 0 then
//                                // no fresh nested subtypes, erase
//                                types.Remove(t) |> ignore
//                            else
//                                // has fresh nested subtypes, keep type for scaffolding but erase contents
//                                yield! freshNested
//                                do eraseTypeContents t
//                        else
//                            // new type, collect the entire nested hierarchy
//                            yield! gatherHierarchy reflectedType t
//            }
//
//        and gatherHierarchy (rt : Type) (t : TypeDefinition) =
//            seq {
//                yield (rt, t)
//                for nt in t.NestedTypes do
//                    let rnt = rt.GetNestedType(nt.Name, BindingFlags.NonPublic ||| BindingFlags.Public)
//                    yield! gatherHierarchy rnt nt
//            }
//
//        traverseTypeDefinitions assemblyDef.MainModule.Types |> Seq.toArray
//
//    let eraseStaticInitializers (state : DynamicAssemblyState) (types : (Type * TypeDefinition) []) =
//
//        let erase (reflectedType : Type, typeDef : TypeDefinition) =
//            // static initializers for generic types not supported
//            if typeDef.GenericParameters.Count > 0 then [||]
//            else
//                let eraseCctor = failwith "" //state.Profile.EraseStaticConstructor reflectedType
//                if eraseCctor then
//                    match typeDef.Methods |> Seq.tryFind(fun m -> m.Name = ".cctor") with
//                    | Some cctor -> typeDef.Methods.Remove(cctor) |> ignore
//                    | None -> ()
//
//                reflectedType.GetFields(BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
//                |> Array.filter (fun f -> not f.IsLiteral && state.Profile.PickleStaticField (f, eraseCctor))
//
//        Array.collect erase types
//
//    /// type reference updating logic : consult the compiler state to update TypeReferences to dynamic assemblies
//
//    let tryUpdateTypeReference (state : DynamicAssemblyCompilerState) (assembly : AssemblyDefinition) (t : TypeReference) =
//        if t = null then None
//        else
//            let assemblyName = defaultArg t.ContainingAssembly assembly.FullName
//
//            match state.DynamicAssemblies.TryFind assemblyName with
//            | None -> None // referenced assembly not dynamic
//            | Some info ->
//                let name = t.CanonicalName
//                match info.TypeIndex.TryFind name with
//                | None when assemblyName = assembly.FullName -> None // the type will be compiled in current slice; do not remap
//                | None -> failwithf "Vagrant: referencing type '%O' from dynamic assembly '%s' which has not been sliced." name assemblyName
//                | Some sliceInfo -> failwith ""
////                    let rt = sliceInfo.Assembly.GetType(name, true)
////                    let tI = assembly.MainModule.Import(rt)
////                    Some tI


    /// compiles a slice of given dynamic assembly snapshot

    let compileDynamicAssemblySlice (state : DynamicAssemblyCompilerState)
                                    (assemblyState : DynamicAssemblyState)
                                    (typeData : Map<string, TypeParseInfo>)
                                    (slice : AssemblyDefinition) =

        // prepare slice info
        let sliceId = assemblyState.GeneratedSlices.Count + 1
        let name = state.CreateAssemblySliceName assemblyState.Name.Name sliceId
        let target = System.IO.Path.Combine(state.OutputDirectory, name + ".dll")

        // update assembly name & write to disk
        do slice.Name.Name <- name
        do slice.Write(target)

        // load new slice to System.Reflection
        let assembly = Assembly.ReflectionOnlyLoadFrom(target)
        
        // collect pickleable static fields
        let pickleableFields = 
            typeData 
            |> Seq.map (function KeyValue(_,InCurrentSlice(_,fields)) -> fields | _ -> [||] ) 
            |> Array.concat

        let sliceInfo = 
            { 
                SourceId = state.ServerId
                Assembly = assembly 
                DynamicAssemblyQualifiedName = assemblyState.DynamicAssembly.FullName 
                SliceId = sliceId 
                StaticFields = pickleableFields
            }

        // update type index & compiled assembly info
        let generatedSlices = assemblyState.GeneratedSlices.Add(sliceId, sliceInfo)
        
        let mapTypeIndex (id : string) (info : TypeParseInfo) =
            match info with
            | AlwaysIncluded -> InAllSlices
            | InCurrentSlice _ -> InSpecificSlice sliceInfo
            | InPastSlice (slice = slice) -> InSpecificSlice slice
            | Erased -> InNoSlice

        let typeIndex = typeData |> Map.map mapTypeIndex
        let assemblyState = { assemblyState with GeneratedSlices = generatedSlices ; TypeIndex = typeIndex }
        let dynamicAssemblyIndex = state.DynamicAssemblies.Add(assemblyState.DynamicAssembly.FullName, assemblyState)
        let state = { state with DynamicAssemblies = dynamicAssemblyIndex}

        sliceInfo, state



    /// compiles a collection of assemblies

    let compileDynamicAssemblySlices (state : DynamicAssemblyCompilerState) (assemblies : Assembly list) =
        // resolve dynamic assembly dependency graph
        let parsedDynamicAssemblies = parseDynamicAssemblies state assemblies

        // exceptions are handled explicitly so that returned state reflects the last successful compilation
        let compileSlice (state : DynamicAssemblyCompilerState, accumulator : Exn<DynamicAssemblySlice list>)
                            (typeData, dynAsmb, _, assemblyDef) =

            match accumulator with
            | Success slices ->
                try
                    let slice, state = compileDynamicAssemblySlice state dynAsmb typeData assemblyDef
                    state, Success (slice :: slices)
                with e ->
                    state, Error e
            | Error _ -> state, accumulator

        List.fold compileSlice (state, Success []) parsedDynamicAssemblies

    /// initializes a stateful compilation agent

    let mkCompilationAgent (profiles : IDynamicAssemblyProfile list) (outpath : string) =
        let init = initCompilerState profiles outpath 
        mkStatefulAgent init compileDynamicAssemblySlices