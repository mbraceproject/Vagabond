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

    // Response given by the Vagrant client upon loading a PortableAssembly

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
        abstract EraseStaticConstructor : Type -> bool

        /// Specifies if static field is to be pickled
        abstract PickleStaticField : FieldInfo * isErasedCtor : bool -> bool

        /// Decides if given slices requires fresh evaluation of assemblies
        abstract IsPartiallyEvaluatedSlice : sliceResolver : (Type -> Assembly option) -> Assembly -> bool


    // internal compiler data structures

    type internal DynamicTypeInfo =
        | InNoSlice
        | InAllSlices
        | InSpecificSlice of DynamicAssemblySlice

    and internal DynamicAssemblySlice =
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
            Profile : IDynamicAssemblyProfile
            GeneratedSlices : Map<int, DynamicAssemblySlice>
            TypeIndex : Map<string, DynamicTypeInfo>
        }
    with
        member s.HasFreshTypes =
            let assemblyTypeCount = s.DynamicAssembly.GetTypes().Length
            let compiledTypeCount = s.TypeIndex.Count
            assemblyTypeCount > compiledTypeCount

        member s.Name = s.DynamicAssembly.GetName()

        member s.LatestSlice = s.GeneratedSlices.TryFind s.GeneratedSlices.Count

        member i.TryGetSlice(t : Type) =
            match i.TypeIndex.TryFind t.FullName with
            | None -> None
            | Some (InNoSlice | InAllSlices) -> failwithf "Vagrant error: type '%O' does not correspond to a slice." t
            | Some (InSpecificSlice s) -> Some s

        static member Init(a : Assembly, profile : IDynamicAssemblyProfile) =
            {
                DynamicAssembly = a
                Profile = profile
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