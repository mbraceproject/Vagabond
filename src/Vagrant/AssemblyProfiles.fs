namespace Nessos.Vagrant

    open System
    open System.Reflection
    open System.Text.RegularExpressions

    type internal DefaultDynamicAssemblyProfile () =
        interface IDynamicAssemblyProfile with
            member __.IsMatch _ = raise <| new NotSupportedException()
            member __.Description = "The default dynamic assembly profile."

            member __.AlwaysIncludeType _ = false
            member __.EraseType _ = false
            member __.EraseStaticConstructor _ = false
            member __.PickleStaticField (_,_) = false
            member __.IsPartiallyEvaluatedSlice _ _ = false


    type FsiDynamicAssemblyProfile () =

        let fsiAssemblyName = "FSI-ASSEMBLY"
        let fsiRegex = new Regex("^FSI_([0-9]{4})$")
        let fsiAssembly = ref None

        let tryGetCurrentInteractionType () =
            match !fsiAssembly with
            | None ->
                let result =
                    System.AppDomain.CurrentDomain.GetAssemblies() 
                    |> Array.tryFind(fun a -> a.IsDynamic && a.GetName().Name = fsiAssemblyName)
                fsiAssembly := result
            | _ -> ()

            match !fsiAssembly with
            | None -> None
            | Some a ->
                a.GetTypes()
                |> Seq.choose (fun t ->
                    let m = fsiRegex.Match t.Name 
                    if m.Success then
                        Some(t, int <| m.Groups.[1].Value)
                    else None)
                |> Seq.sortBy (fun (_,id) -> - id)
                |> Seq.tryPick Some
                |> Option.map fst


        interface IDynamicAssemblyProfile with
            member __.IsMatch (a : Assembly) =
                let an = a.GetName() in an.Name = fsiAssemblyName

            member __.Description = "F# Interactive dynamic assembly profile."

            member __.AlwaysIncludeType (t : Type) = 
                t.Name.StartsWith("$") && t.Namespace = null

            member __.EraseType (t : Type) =
                t.Name.StartsWith("$") && 
                    match t.Namespace with
                    | null -> false
                    | ns -> ns.StartsWith("<StartupCode$")

            member __.EraseStaticConstructor (t : Type) =
                Microsoft.FSharp.Reflection.FSharpType.IsModule t

            member __.PickleStaticField (f : FieldInfo, isErasedCctor) =
                isErasedCctor && not <| f.FieldType.Name.StartsWith("$")

            member __.IsPartiallyEvaluatedSlice (sliceResolver : Type -> Assembly option) (slice : Assembly) =
                tryGetCurrentInteractionType () 
                |> Option.bind sliceResolver
                |> Option.exists (fun a -> a = slice)