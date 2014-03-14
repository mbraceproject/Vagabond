namespace Nessos.Vagrant

    open System
    open System.Reflection

    type internal DefaultDynamicAssemblyProfile () =
        interface IDynamicAssemblyProfile with
            member __.IsMatch _ = raise <| new NotSupportedException()
            member __.Description = "The default dynamic assembly profile."

            member __.AlwaysIncludeType _ = false
            member __.EraseType _ = false
            member __.EraseStaticConstructor _ = false
            member __.PickleStaticField (_,_) = false


    type FsiDynamicAssemblyProfile () =
        interface IDynamicAssemblyProfile with
            member __.IsMatch (a : Assembly) =
                let an = a.GetName() in an.Name = "FSI-ASSEMBLY"

            member __.Description = "F# Interactive dynamic assembly profile."

            member __.AlwaysIncludeType (t : Type) = 
                t.Name.StartsWith("$") && t.Namespace = null

            member __.EraseType (t : Type) =
                t.Name.StartsWith("$") && t.Namespace.StartsWith("<StartupCode$")

            member __.EraseStaticConstructor (t : Type) =
                Microsoft.FSharp.Reflection.FSharpType.IsModule t

            member __.PickleStaticField (f : FieldInfo, isErasedCctor) =
                isErasedCctor && not <| f.FieldType.Name.StartsWith("$")