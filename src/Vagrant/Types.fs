namespace Nessos.Vagrant

    open System
    open System.Reflection

    open Nessos.FsPickler

    type DynamicAssemblySlice =
        {
            SourceId : Guid
            DynamicAssemblyQualifiedName : string

            SliceId : int
            Assembly : Assembly

            StaticFields : FieldInfo []
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

        /// Decides if given slices requires fresh evaluation of assemblies
        abstract IsPartiallyEvaluatedSlice : sliceResolver : (Type -> DynamicAssemblySlice option) -> DynamicAssemblySlice -> bool

    and PortableAssembly =
        {
            FullName : string
            Image : byte [] option
            DynamicAssemblyInfo : DynamicAssemblyInfo option
        }

    and DynamicAssemblyInfo =
        {
            SourceId : Guid
            DynamicAssemblyName : string
            SliceId : int

            // static initialization data
            IsPartiallyEvaluated : bool
            Generation : int
            StaticInitializationData : StaticInitializationData option
        }

    and StaticInitializationData =
        {
            Pickles : (FieldInfo * byte []) []
            Errors : (FieldInfo * exn) []
        }

    and AssemblyResponse =
        | Loaded
        | MissingImage
        | MissingStaticInitializer
//        | MissingImage of assemblyName:string
//        | MissingStaticInitializer of assemblyName:string


    and internal DynamicAssemblyState =
        {
            DynamicAssembly : Assembly
            AssemblyReferences : Assembly list
            Profile : IDynamicAssemblyProfile
            GeneratedSlices : Map<int, DynamicAssemblySlice>
            TypeIndex : Map<string, DynamicAssemblySlice>
        }
    with
        member i.HasFreshTypes =
            let assemblyTypeCount = i.DynamicAssembly.GetTypes().Length
            let compiledTypeCount = i.TypeIndex.Count
            assemblyTypeCount > compiledTypeCount

        member i.Name = i.DynamicAssembly.GetName()

        member i.LatestSlice = i.GeneratedSlices.TryFind i.GeneratedSlices.Count

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
            Profiles : IDynamicAssemblyProfile list
            OutputDirectory : string

            DynamicAssemblies : Map<string, DynamicAssemblyState>
            
            TryGetDynamicAssemblyId : string -> (string * int) option
            CreateAssemblySliceName : string -> int -> string
        }
    with
        member s.TryFindSliceInfo(sliceName : string) =
            match s.TryGetDynamicAssemblyId sliceName with
            | Some(dynamicAssemblyName, id) ->
                let an = new AssemblyName(sliceName)
                do an.Name <- dynamicAssemblyName
                match s.DynamicAssemblies.TryFind an.FullName with
                | None -> None
                | Some info -> info.GeneratedSlices.TryFind id |> Option.map (fun slice -> info, slice)
            | None -> None