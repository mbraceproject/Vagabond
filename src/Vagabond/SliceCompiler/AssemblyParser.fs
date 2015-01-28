module internal Nessos.Vagabond.AssemblyParser

    open System
    open System.Reflection
    open System.Collections.Generic

    open Mono.Cecil

    open Nessos.Vagabond
    open Nessos.Vagabond.Utils
    open Nessos.Vagabond.Cecil
    open Nessos.Vagabond.SliceCompilerTypes

    type TypeParseInfo =
        | AlwaysIncluded
        | InCurrentSlice of eraseCctor:bool * pickledFields:FieldInfo[]
        | InPastSlice of slice:DynamicAssemblySlice * containsNestedTypeInCurrentSlice:bool
        | Erased

    /// traverses a dynamic assembly and compiles and index of parse information
    /// to be used for the assembly parser configuration

    let computeSliceData (state : DynamicAssemblyState) =

        let rec getParseInfo (parent : TypeParseInfo option) (map : Map<string, TypeParseInfo>) (t : Type) =

            let update map (t : Type) (info : TypeParseInfo) = 
                let map = Map.add t.FullName info map
                let nested = t.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic)
                getParseInfos (Some info) map nested

            match parent with
            // Erased & AlwaysIncluded propagate upwards in the nested type hierarchy
            | Some Erased -> update map t Erased
            | Some AlwaysIncluded -> update map t AlwaysIncluded
            | _ ->
                match state.TypeIndex.TryFind t.FullName with
                // maintain Erased & AlwaysIncluded types if so declared in earlier slices
                | Some InNoSlice -> update map t Erased
                | Some InAllSlices -> update map t AlwaysIncluded
                // keep record of earlier slice ownership; start by declaring that type contains no new nested types
                // this is to be updated later if new nested types are discovered
                | Some (InSpecificSlice slice) -> update map t (InPastSlice (slice, false))
                | None ->
                    // new type, compute information

                    if t.Namespace = null && t.DeclaringType = null && t.Name = "[<Module>]" then
                        update map t AlwaysIncluded

                    elif state.Profile.EraseType t then
                        update map t Erased

                    elif state.Profile.AlwaysIncludeType t then
                        update map t AlwaysIncluded

                    else
                        // determine emitted type metadata
                        let eraseCctor = state.Profile.EraseStaticConstructor t
                        let pickledFields =
                            // static initializers for generic types not supported
                            if t.IsGenericTypeDefinition then [||]
                            else
                                t.GetFields(BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                                |> Array.filter (fun f -> not f.IsLiteral && state.Profile.PickleStaticField (f, eraseCctor))

                        // in case of nested type with parent belonging to previous slice,
                        // update parent info to reflect this information
                        let rec updateParents (map : Map<string, TypeParseInfo>) (t : Type) =
                            match t.DeclaringType with
                            | null -> map
                            | dt ->
                                match map.[dt.FullName] with
                                | InPastSlice(slice, false) ->
                                    let map = Map.add dt.FullName (InPastSlice(slice,true)) map
                                    updateParents map t
                                | _ -> map

                        let map = updateParents map t

                        update map t <| InCurrentSlice(eraseCctor, pickledFields)            

        and getParseInfos parent map types =
            Array.fold (getParseInfo parent) map types

        state.DynamicAssembly.GetTypes() 
        |> Array.filter (fun t -> not t.IsNested)
        |> getParseInfos None Map.empty
    
    // used by the assembly parser to remap references to corresponding slices

    let tryRemapReferencedType (state : DynamicAssemblyCompilerState) (t : Type) =
        match state.DynamicAssemblies.TryFind t.Assembly.FullName with
        | None -> None
        | Some dynInfo ->
            match dynInfo.TypeIndex.TryFind t.FullName with
            | None | Some InAllSlices -> None
            | Some InNoSlice ->
                raise <| new VagabondException(sprintf "could not compile slice; referenced excluded type '%O' in assembly '%O'." t dynInfo.Name)
            | Some (InSpecificSlice slice) -> 
                Some <| slice.Assembly.GetType(t.FullName, true)
        

    /// the main assembly parsing method

    let parseDynamicAssemblySlice (state : DynamicAssemblyCompilerState) (assembly : Assembly) =

        // resolve dynamic assembly state
        let assemblyState =
            match state.DynamicAssemblies.TryFind assembly.FullName with
            | None -> 
                let profile =
                    match state.Profiles |> List.tryFind(fun p -> p.IsMatch assembly) with
                    | Some p -> p
                    | None -> new DefaultDynamicAssemblyProfile() :> _
                DynamicAssemblyState.Init(assembly, profile)

            | Some info when info.DynamicAssembly <> assembly ->
                raise <| new VagabondException(sprintf "ran into duplicate dynamic assemblies of qualified name '%s'. This is not supported." assembly.FullName)

            | Some info -> info
        
        let typeInfo = computeSliceData assemblyState

        let remap = memoize (tryRemapReferencedType state)

        // configuration to be passed to the parser

        let parseConfiguration =
            {
                new IAssemblyParserConfig with
                    member __.EraseMember (m : MemberInfo) =
                        match m with
                        // erase static constructor if so specified
                        | :? ConstructorInfo as c when c.IsStatic ->
                            match typeInfo.TryFind c.DeclaringType.FullName with
                            | Some (InCurrentSlice(eraseCctor = true)) -> true
                            | _ -> false
                        | _ -> false

                    member __.GetTypeParseAction (t : Type) =
                        match typeInfo.TryFind t.FullName with
                        | None -> TypeParseAction.Ignore
                        | Some (InCurrentSlice _) -> TypeParseAction.ParseAll
                        | Some (InPastSlice (containsNestedTypeInCurrentSlice = true)) -> TypeParseAction.ParseNested
                        | Some (InPastSlice _) -> TypeParseAction.Ignore
                        | Some (Erased _) -> TypeParseAction.Ignore
                        | Some (AlwaysIncluded _) -> TypeParseAction.ParseAll

                    // make *all* members in the slice public.
                    // this is essential to ensure seamless referencing between slices
                    member __.MakePublic _ = true

                    member __.RemapReference(t : Type, outType : byref<Type>) =
                        match remap t with
                        | None -> false
                        | Some t' -> outType <- t'; true
            }

        let sliceDefinition = AssemblyParser.Parse(assembly, parseConfiguration)

        let dependencies = 
            sliceDefinition.MainModule.AssemblyReferences
            |> Seq.map (fun r -> r.FullName)
            |> Seq.distinct
            |> Seq.toList

        typeInfo, assemblyState, dependencies, sliceDefinition