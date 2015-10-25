module internal Nessos.Vagabond.SliceCompilerTypes

open System
open System.Reflection

// internal compiler data structures

/// Contains information on compiled dynamic assembly slice
type DynamicAssemblySlice =
    {
        /// Qualified name of original dynamic assembly
        DynamicAssemblyQualifiedName : string
        /// Slice serial number
        SliceId : int
        /// System.Reflection.Assembly to slice
        Assembly : Assembly
        /// Static fields that are to be pickled by slice
        StaticFields : FieldInfo []
    }
with
    /// Returns true if slice requires static initialization of fields
    member slice.RequiresStaticInitialization = slice.StaticFields.Length > 0


/// Dynamic type metadata w.r.t. its containing static slice
type DynamicTypeInfo =
    | InNoSlice
    | InAllSlices
    | InSpecificSlice of DynamicAssemblySlice

/// Dynamic assembly compilation state
type DynamicAssemblyState =
    {
        /// Original dynamic assembly
        DynamicAssembly : Assembly
        /// User-supplied profile used for guiding slice compilation
        Profile : IDynamicAssemblyProfile
        /// Generated slices indexed by serial number
        GeneratedSlices : Map<int, DynamicAssemblySlice>
        /// Dynamic type information indexed by name
        TypeIndex : Map<string, DynamicTypeInfo>
    }
with
    /// System.Reflection.AssemblyName for dynamic assembly
    member s.Name = s.DynamicAssembly.GetName()
    /// Returns the latest generated slice for dynamic assembly
    member s.LatestSlice = s.GeneratedSlices.TryFind s.GeneratedSlices.Count

    /// Returns true if dynamic assembly has new types that need to be compiled to slice
    member s.HasFreshTypes =
        let currentTypeCount =
            if not runsOnMono.Value then
                s.DynamicAssembly.GetTypes().Length
            else
                // mono needs different approach since Assembly.GetTypes() only returns top-level types
                let count = ref 0
                let rec countTypes (types : Type []) =
                    count := !count + types.Length
                    for t in types do
                        countTypes <| t.GetNestedTypes(BindingFlags.NonPublic ||| BindingFlags.Public)

                countTypes <| s.DynamicAssembly.GetTypes()
                !count

        let compiledTypeCount = s.TypeIndex.Count
        currentTypeCount > compiledTypeCount

    /// Initialize a dynamic assembly state
    static member Init(a : Assembly, profile : IDynamicAssemblyProfile) =
        {
            DynamicAssembly = a
            Profile = profile
            GeneratedSlices = Map.empty
            TypeIndex = Map.empty
        }

/// Represents the state of an in-memory assembly that has been compiled to disk
type CompiledInMemoryAssembly =
    {
        Origin : Assembly
        CompiledAssembly : Assembly
    }

/// Global assembly compiler state
type AssemblyCompilerState =
    {
        /// Unique compiler identifier
        CompilerId : Guid
        /// User-supplied dynamic assembly profiles
        Profiles : IDynamicAssemblyProfile []
        /// Output directory used for slices
        OutputDirectory : string
        /// List of currently compiled dynamic assemblies, indexed by qualified name
        DynamicAssemblies : Map<string, DynamicAssemblyState>
        /// List of currently compiled in-memory assemblies, indexed by qualified name
        InMemoryAssemblies : Map<string, CompiledInMemoryAssembly>
        /// Parses a slice assembly qualified name, returning the dynamic assembly qualified name and slice id.
        TryGetDynamicAssemblyId : string -> (string * int) option
        /// Creates a slice assembly qualified name out of a dynamic assembly qualified name and slice id.
        CreateAssemblySliceName : string -> int -> string
    }
with
    /// Returns contained dynamic assembly slice state using supplied slice qualified name
    member s.TryFindSliceInfo(sliceName : string) =
        match s.TryGetDynamicAssemblyId sliceName with
        | Some(dynamicAssemblyName, sliceId) ->
            let assemblyName = new AssemblyName(sliceName)
            do assemblyName.Name <- dynamicAssemblyName
            match s.DynamicAssemblies.TryFind assemblyName.FullName with
            | None -> None
            | Some info -> info.GeneratedSlices.TryFind sliceId |> Option.map (fun slice -> info, slice)
        | None -> None

    /// Returns true if local dynamic assembly slice id.
    member s.IsLocalDynamicAssemblySlice (id : AssemblyId) = 
        s.TryGetDynamicAssemblyId(id.FullName).IsSome