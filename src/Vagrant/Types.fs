namespace Nessos.Vagrant

    open System
    open System.Reflection


    type DependencyInfo =
        {
            Assembly : Assembly
            SourceId : Guid
            IsDynamicAssemblySlice : bool
            DynamicAssemblyQualifiedName : string
            SliceId : int
            BlobGeneration : int
            TypeInitializationBlobs : (FieldInfo * byte []) list
            TypeInitializationErrors : (FieldInfo * string) list
        }


    /// customizes slicing behaviour on given dynamic assembly
    and IDynamicAssemblyProfile =

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


    and internal AssemblySliceInfo =
        {
            Assembly : Assembly
            DynamicAssemblyName : string
            SliceId : int
            StaticFields : (FieldInfo * FieldInfo) list
        }

    and internal DynamicAssemblyState =
        {
            DynamicAssembly : Assembly
            AssemblyReferences : Assembly list
            Profile : IDynamicAssemblyProfile
            GeneratedSlices : Map<string, AssemblySliceInfo>
            TypeIndex : Map<string, AssemblySliceInfo>
        }
    with
        member i.HasFreshTypes =
            let assemblyTypeCount = i.DynamicAssembly.GetTypes().Length
            let compiledTypeCount = i.TypeIndex.Count
            assemblyTypeCount > compiledTypeCount

        member i.Name = i.DynamicAssembly.GetName()

        static member Init(a : Assembly, profile : IDynamicAssemblyProfile) =
            {
                DynamicAssembly = a
                Profile = profile
                AssemblyReferences = []
                GeneratedSlices = Map.empty
                TypeIndex = Map.empty
            }

    and internal DynamicAssemblyCompilerState =
        {
            ServerId : Guid
            OutputDirectory : string
            DynamicAssemblies : Map<string, DynamicAssemblyState>
            Profiles : IDynamicAssemblyProfile list

            TryGetDynamicAssemblyName : string -> string option
            CreateAssemblySliceName : string -> int -> string
        }
    with
        member s.TryFindSliceInfo(sliceName : string) =
            match s.TryGetDynamicAssemblyName sliceName with
            | Some name ->
                let an = new AssemblyName(sliceName)
                do an.Name <- name
                s.DynamicAssemblies.TryFind(an.FullName) 
                |> Option.bind(fun info -> info.GeneratedSlices.TryFind sliceName)
            | None -> None