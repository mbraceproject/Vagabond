namespace Nessos.Vagrant

    open System
    open System.Reflection

    open Nessos.FsPickler

    /// Contains information necessary for the exportation of an assembly
    type PortableAssembly =
        {
            /// The Qualified name of the assembly
            FullName : string

            /// Raw image of the assembly
            Image : byte [] option

            /// Information on originating dynamic assembly
            DynamicAssemblyInfo : DynamicAssemblyInfo option
        }

    and DynamicAssemblyInfo =
        {
            /// Unique identifier of dynamic assembly source
            SourceId : Guid

            /// Original dynamic assembly qualified name
            DynamicAssemblyName : string

            /// Identifier of current slice
            SliceId : int

            /// Slice contains static fields that require initialization
            RequiresStaticInitialization : bool

            /// Dynamic assembly requires further evaluation ; 
            /// will require static initialization revision in the future.
            IsPartiallyEvaluated : bool

            /// Generation of latest static initialization data
            StaticInitializerGeneration : int option

            /// Static initialization data
            StaticInitializerData : byte [] option
        }


    and AssemblyLoadResponse =
        | Loaded of string * (FieldInfo * exn) []
        | LoadFault of string * exn
        | MissingAssemblyImage of string
        | MissingStaticInitializer of string * int option
    with
        member r.FullName = match r with | Loaded(name,_) | LoadFault(name,_) | MissingAssemblyImage(name) | MissingStaticInitializer(name,_) -> name
        member r.StaticInitializationErrors = match r with | Loaded(_,errors) -> errors | _ -> [||]



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
        abstract EraseStaticConstructor : ConstructorInfo -> bool

        /// Specifies if static field is to be pickled
        abstract PickleStaticField : FieldInfo * isErasedCtor : bool -> bool

        /// Decides if given slices requires fresh evaluation of assemblies
        abstract IsPartiallyEvaluatedSlice : sliceResolver : (Type -> Assembly option) -> Assembly -> bool


    // internal compiler data structures

    type internal DynamicAssemblySlice =
        {
            SourceId : Guid
            DynamicAssemblyQualifiedName : string

            SliceId : int
            Assembly : Assembly

            StaticFields : FieldInfo []
        }

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