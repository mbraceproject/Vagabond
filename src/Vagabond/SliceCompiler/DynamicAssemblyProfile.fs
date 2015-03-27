namespace Nessos.Vagabond

open System
open System.Reflection
open System.Text.RegularExpressions

/// customizes slicing behaviour on given dynamic assembly
type IDynamicAssemblyProfile =

    /// identifies dynamic assemblies that match this profile
    abstract IsMatch : Assembly -> bool

    /// a short description of the profile
    abstract Description : string
        
    /// Specifies if type is to be included in every iteration of the slice
    abstract AlwaysIncludeType: Type -> bool

    /// Specifies if type is to be erased from slices
    abstract EraseType : Type -> bool

    /// Specifies if static constructor is to be erased
    abstract EraseStaticConstructor : Type -> bool

    /// Specifies if static field is to be pickled
    abstract PickleStaticField : FieldInfo * isErasedCtor : bool -> bool


/// Default dynamic assembly profile; no erasure, no static initialization.
type internal DefaultDynamicAssemblyProfile () =
    interface IDynamicAssemblyProfile with
        member __.IsMatch _ = raise <| new NotSupportedException()
        member __.Description = "The default dynamic assembly profile."

        member __.AlwaysIncludeType _ = false
        member __.EraseType _ = false
        member __.EraseStaticConstructor _ = false
        member __.PickleStaticField (_,_) = false

/// Dynamic Assembly profile for F# Interactive
type FsiDynamicAssemblyProfile () =

    let fsiAssemblyName = "FSI-ASSEMBLY"

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