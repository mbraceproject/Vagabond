namespace Nessos.DistribFsi

    open System
    open System.Reflection

    open FsPickler

    type DeclarationId = string list
    
    and Declaration =
        | Fsi of (* top-level module name *) string * DeclarationId
        | External of Assembly * DeclarationId
    with
        member r.Id = match r with Fsi (_,id) -> id | External(_,id) -> id

    and DeclarationType =
        | Method
        | UnsafeProperty
        | PickledProperty
        | DeletedProperty

    and FsiDeclarationInfo =
        {
            Id : DeclarationId
            ModuleName : string
            DeclarationType : DeclarationType
            Dependencies : Declaration list
        }

    and FsiCompiledAssembly =
        {
            Name : string
            FullName : string
            Location : string
            PdbLocation : string option
        }

    and 
        [<CustomEquality>]
        [<CustomComparison>]
        AssemblyDescriptor =
        | StaticAssembly of Assembly
        | FsiTypeProvider of Assembly
        | FsiCompiled of FsiCompiledAssembly
    with
        member d.Name = match d with StaticAssembly a | FsiTypeProvider a -> a.GetName().Name | FsiCompiled f -> f.Name
        member d.FullName = match d with StaticAssembly a | FsiTypeProvider a -> a.FullName | FsiCompiled f -> f.FullName
        member d.Location = 
            match d with 
            | StaticAssembly a
            | FsiTypeProvider a -> a.Location 
            | FsiCompiled f -> f.Location // will result in exception  

        member d.GlobalAssemblyCache = match d with StaticAssembly a -> a.GlobalAssemblyCache | _ -> false

        member private d.Comparable =
            match d with
            | StaticAssembly a -> Choice1Of3 a.Location
            | FsiTypeProvider a -> Choice2Of3 a.FullName
            | FsiCompiled f -> Choice3Of3 f.Location

        member x.CompareTo (y : obj) =
            match y with
            | :? AssemblyDescriptor as y -> compare x.Comparable y.Comparable
            | _ -> invalidArg "invalid comparand" "y"

        override x.Equals y = x.CompareTo y = 0
        override x.GetHashCode () = x.Comparable.GetHashCode()
        interface IComparable with member x.CompareTo y = x.CompareTo y


    and ReadOnly<'T> = abstract Value : 'T

    // TODO : fill in
    and InteractionCompiler = { Pickler : FsPickler }