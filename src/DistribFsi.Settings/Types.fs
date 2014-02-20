namespace Nessos.DistribFsi

    open System
    open System.IO
    open System.Reflection

    type DeclarationId = { Path : string list }
    with
        member i.Name = let p = i.Path in p.[p.Length - 1]
        member i.TopLevelName = i.Path.Head

        static member Create(path : string, ?name : string) =
            { Path = 
                [ 
                    yield! path.Split('`').[0].Split([|'.';'+'|]) 
                    match name with None -> () | Some n -> yield n 
                ] }
            
        static member OfMemberInfo (m : MemberInfo) = DeclarationId.Create(m.DeclaringType.FullName, m.Name)
        static member OfType (t : Type) = DeclarationId.Create(t.FullName)

    and FsiCompiledAssembly =
        {
            Assembly : Assembly
            PdbFile : string option
            Interactions : FsiInteractionInfo list
            Dependencies : Assembly list
        }
    with
        member __.AssemblyName = __.Assembly.GetName()
        member __.Location = __.Assembly.Location

    and FsiInteractionInfo =
        {
            Name : string
            ContainingAssembly : AssemblyName
            Declarations : Map<DeclarationId, FsiDeclarationInfo>
        }

    and FsiDeclarationInfo =
        {
            Id : DeclarationId
            MemberInfo : MemberInfo
            IsPickledValue : bool
            IsReflectedDefinition : bool
            IsNonSerializableValue : bool
            DefinesCompilerGeneratedTypes : bool
            References : FsiReference list
        }
    with
        member __.IsMethod = match __.MemberInfo with :? MethodInfo -> true | _ -> false
        member __.InteractionName = __.Id.TopLevelName
        member __.DeclaringType = __.MemberInfo.DeclaringType
        member __.DependsOnDynamicAssembly = __.References |> List.exists(fun r -> r.IsDynamic)

    and FsiReference =
        {
            Id : DeclarationId
            DeclaringType : Type
            IsFsiDeclaration : bool
        }
    with
        member __.Assembly = __.DeclaringType.Assembly
        member __.IsDynamic = not __.IsFsiDeclaration && __.IsDynamic

    and DependencyTree = 
        | External of DeclaringType:Type * name:string
        | Fsi of FsiDeclarationInfo * isInCurrentInteraction:bool * requiresCompilation:bool * references:DependencyTree list

    and ObjectDependencyInfo =
        {
            Dependencies : Assembly list option

            DependsOnFsiCompiledAssembly : bool
            DependsOnCurrentInteraction : bool
            RequiresAssemblyCompilation : bool
        }

    and internal InteractionCompilerResult =
        | AlreadyCompiled of FsiCompiledAssembly
        | RequiresCompilation of inCurrentInteraction:bool * AssemblyName

    and internal InteractionCompilerInfo =
        
        abstract ShellId : Guid
        abstract CompiledAssemblyPath : string
        abstract DynamicAssembly : Assembly
        abstract Serializer : ISerializer
        
        abstract RequestCompilation : compileCurrentInteraction:bool -> FsiCompiledAssembly

        abstract TryGetDeclarationInfo : mem:MemberInfo -> DependencyTree option
        abstract TryGetCompiledAssembly : t:Type -> InteractionCompilerResult option