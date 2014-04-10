module internal Nessos.Vagrant.AssemblyParser

    open System
    open System.Reflection
    open System.Collections.Generic

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.Cecil

    type TypeParseInfo =
        | AlwaysIncluded
        | InCurrentSlice of eraseCctor:bool * pickledFields:FieldInfo[]
        | InPastSlice of slice:DynamicAssemblySlice * containsNestedTypeInCurrentSlice:bool
        | Erased

    let computeSliceData (state : DynamicAssemblyState) =

        let rec getParseInfo (parent : TypeParseInfo option) (map : Map<string, TypeParseInfo>) (t : Type) =

            let update map (t : Type) (info : TypeParseInfo) = 
                let map = Map.add t.FullName info map
                let nested = t.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic)
                getParseInfos (Some info) map nested

            match parent with
            | Some Erased -> update map t Erased
            | Some AlwaysIncluded -> update map t AlwaysIncluded
            | _ ->
                match state.TypeIndex.TryFind t.FullName with
                | Some InNoSlice -> update map t Erased
                | Some InAllSlices -> update map t AlwaysIncluded
                | Some (InSpecificSlice slice) -> update map t (InPastSlice (slice, false))
                | None ->
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

                        // in case of declaring type belonging to previous slice, 
                        // update parse info to reflect that a new nested type is contained
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


    let remapMemberReference (state : DynamicAssemblyCompilerState) (remapF : MemberInfo -> MemberInfo) (m : MemberInfo) =
        
        let inline remap (m : 'M) = (remapF m) :?> 'M

        match m with
        | :? Type as t ->
            if t.IsGenericType && not t.IsGenericTypeDefinition then
                let gt = t.GetGenericTypeDefinition() |> remap
                let gas = t.GetGenericArguments() |> Array.map remap
                gt.MakeGenericType gas :> MemberInfo

            elif t.IsGenericParameter then
                let parameters =
                    match t.DeclaringMethod with
                    | null -> remap(t.DeclaringType).GetGenericArguments()
                    | dm -> remap(dm).GetGenericArguments()

                parameters |> Array.find(fun t' -> t'.Name = t'.Name) :> MemberInfo
            else
                // is named type
                match state.DynamicAssemblies.TryFind t.Assembly.FullName with
                | None -> t :> MemberInfo
                | Some dynInfo ->
                    match dynInfo.TypeIndex.TryFind t.FullName with
                    | None | Some InAllSlices -> t :> MemberInfo
                    | Some InNoSlice -> 
                        failwithf "Vagrant: error compiling slice: referenced excluded type '%O' in assembly '%O'." t dynInfo.Name
                    | Some (InSpecificSlice slice) ->
                        slice.Assembly.GetType(t.FullName, true) :> MemberInfo

        | :? MethodInfo as m -> //as m when m.IsGenericMethod && not m.IsGenericMethodDefinition ->
            if m.IsGenericMethod && not m.IsGenericMethodDefinition then
                let gm = m.GetGenericMethodDefinition() |> remap
                let ga = m.GetGenericArguments() |> Array.map remap
                gm.MakeGenericMethod ga :> MemberInfo
            else
                let dt = remap m.DeclaringType
                dt.GetMethods(allBindings)
                |> Array.find (fun m' -> m'.IsStatic = m.IsStatic && m'.ToString() = m.ToString())
                :> MemberInfo

        | :? ConstructorInfo as c ->
            let dt = remap m.DeclaringType
            dt.GetConstructors(allBindings)
            |> Array.find (fun c' -> c.ToString() = c'.ToString())
            :> MemberInfo

        | m ->
            let dt = remap m.DeclaringType
            dt.GetMember(m.Name, allBindings).[0]


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

            | Some info -> info
        
        let typeInfo = computeSliceData assemblyState

        let dependencies = new HashSet<Assembly> ()
        let remapRef =
            Ymemoize (fun f (m : MemberInfo) ->
                if m.Assembly <> assembly then 
                    dependencies.Add m.Assembly |> ignore

                remapMemberReference state f m)

        let parseConfig =
            {
                new IAssemblyParserConfig with
                    member __.EraseMember (m : MemberInfo) =
                        match m with
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

                    member __.MakePublic _ = true
                    member __.RemapReference m = remapRef m
            }

        let sliceDefinition = AssemblyParser.Parse(assembly, parseConfig)

        let dependencies = Seq.toList dependencies

        typeInfo, assemblyState, dependencies, sliceDefinition