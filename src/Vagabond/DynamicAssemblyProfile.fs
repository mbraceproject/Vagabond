namespace Nessos.Vagabond

open System
open System.Reflection
open System.Text.RegularExpressions

open Microsoft.FSharp.Reflection

/// Determines dynamic type parsing behaviour
/// to Vagabond slices
type TypeParseBehaviour =
    /// Include new type to current slice
    | InCurrentSlice
    /// Include type in every slice
    | InEverySlice
    /// Erase type from all slices
    | InNoSlice 

/// customizes slicing behaviour on given dynamic assembly
type IDynamicAssemblyProfile =

    /// identifies dynamic assemblies that match this profile
    abstract IsMatch : Assembly -> bool

    /// a short description of the profile
    abstract Description : string
        
    /// Specifies slice parse behaviour for provided type.
    abstract GetTypeParseBehaviour : Type -> TypeParseBehaviour

    /// Specifies if static constructor is to be erased.
    abstract EraseStaticConstructor : Type -> bool

    /// <summary>
    ///     Specifies if static field value is to be pickled for remote party.
    /// </summary>
    /// <param name="field">Field to be checked.</param>
    /// <param name="isErasedCctor">Specifies if static constructor has been erased for declaring type.</param>
    abstract PickleStaticField : field:FieldInfo * isErasedCctor:bool -> bool


/// Default dynamic assembly profile; no erasure, no static initialization.
type internal DefaultDynamicAssemblyProfile () =
    interface IDynamicAssemblyProfile with
        member __.IsMatch _ = raise <| new NotSupportedException()
        member __.Description = "The default dynamic assembly profile."
        member __.GetTypeParseBehaviour _ = InCurrentSlice
        member __.EraseStaticConstructor _ = false
        member __.PickleStaticField (_,_) = false

/// Dynamic Assembly profile for F# Interactive
type FsiDynamicAssemblyProfile () =

    let fsiAssemblyName = "FSI-ASSEMBLY"

    interface IDynamicAssemblyProfile with
        member __.IsMatch (a : Assembly) =
            let an = a.GetName() 
            an.Name = fsiAssemblyName && 
                an.Version.ToString() = "0.0.0.0" &&
                    a.GetType("FSI_0001") <> null 

        member __.Description = "F# Interactive dynamic assembly profile."

        member __.GetTypeParseBehaviour (t : Type) =
            // Need to keep all '$ArrayType$844' type declarations in every compiled slice.
            // Everything else is appended normally to current slice.
            if t.Name.StartsWith("$") && t.Namespace = null then InEverySlice
            else
                InCurrentSlice

        member __.EraseStaticConstructor (_ : Type) = false
        member __.PickleStaticField (f : FieldInfo, _ : bool) =
            // all static fields declared in types nested inside modules are to be pickled.
            let rec isModuleType (t : Type) =
                if FSharpType.IsModule t then true
                else
                    match t.DeclaringType with
                    | null -> false
                    | dt -> isModuleType dt
        
            isModuleType f.DeclaringType