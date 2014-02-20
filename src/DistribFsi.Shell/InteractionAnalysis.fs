module internal Nessos.DistribFsi.Shell.InteractionAnalysis

    open System
    open System.Reflection
    open System.Collections.Generic

    open Nessos.DistribFsi
    open Nessos.DistribFsi.Shell.Utils
    open Nessos.DistribFsi.Shell.Types


    let tryBuildDependencyTree (fsiDynamicAssembly : Assembly) (state : InteractionCompilerState) (m : MemberInfo) =

        let isValidDeclaration (id : DeclarationId) =
            // in pending interaction
            state.PendingInteraction |> Option.exists (fun i -> i.Declarations.ContainsKey id) ||
            // in completed interactions
            state.Interactions.TryFind id.TopLevelName |> Option.exists (fun i -> i.Declarations.ContainsKey id)

        let rec traverse (state : InteractionCompilerState) (id : DeclarationId) =
            let declaration, isPending =
                match state.PendingInteraction with
                | Some pending when pending.Name = id.TopLevelName -> pending.Declarations.TryFind id, true
                | _ -> state.Interactions.[id.TopLevelName].Declarations.TryFind id, false

            match declaration with
            | None -> None
            | Some declaration ->

                let requiresCompilation = isPending || state.InteractionQueue |> List.exists(fun i -> i.Name = id.TopLevelName)

                let children = 
                    declaration.References 
                    |> List.choose (fun ref -> 
                        if ref.IsFsiDeclaration then traverse state ref.Id
                        else
                            Some <| External (ref.DeclaringType, ref.Id.Name))

                Some <| Fsi(declaration, isPending, requiresCompilation, children)

        if m.DeclaringType.Assembly = fsiDynamicAssembly then
            let parentId = DeclarationId.OfMemberInfo m
            if isValidDeclaration parentId then
                traverse state parentId
            else None
        else None

    let tryGetCompiledAssembly (fsiDynamicAssembly : Assembly) (state : InteractionCompilerState) (t : Type) =
        if t.Assembly = fsiDynamicAssembly then 
            let moduleName = DeclarationId.OfType(t).TopLevelName
            let isPendingInteraction = 
                match state.PendingInteraction with 
                | Some pending when pending.Name = moduleName -> true 
                | _ -> false

            let requiresCompilation = isPendingInteraction || state.InteractionQueue |> List.exists(fun i -> i.Name = moduleName)

            if requiresCompilation then
                Some <| RequiresCompilation (isPendingInteraction, state.NextCompiledAssembly)
            else
                let an = state.Interactions.[moduleName].ContainingAssembly.Name
                let assembly = state.CompiledAssemblies.[an]
                Some <| AlreadyCompiled assembly
        else
            None