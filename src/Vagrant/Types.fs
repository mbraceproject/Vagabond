namespace Nessos.Vagrant

    open System
    open System.Reflection

    open Nessos.FsPickler

    /// unique identifier for assembly
    type AssemblyId =
        {
            /// Assembly Qualified name
            FullName : string
            /// digest of the raw assembly image
            ImageHash : byte []
        }
    with
        member id.GetName() = new AssemblyName(id.FullName)

    /// Contains information necessary for the exportation of an assembly
    and [<NoEquality;NoComparison>] PortableAssembly =
        {
            // Unique identifier for assembly
            Id : AssemblyId

            /// Raw image of the assembly
            Image : byte [] option

            /// Symbols file
            Symbols : byte [] option

            /// Information on originating dynamic assembly
            DynamicAssemblyInfo : DynamicAssemblyInfo option
        }
    with
        member pa.FullName = pa.Id.FullName
        member pa.GetName() = pa.Id.GetName()
        member pa.HashCode = pa.Id.ImageHash
        

    and [<NoEquality;NoComparison>] DynamicAssemblyInfo =
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
        | Loaded of AssemblyId * (FieldInfo * exn) []
        | LoadFault of AssemblyId * exn
        | MissingAssemblyImage of AssemblyId
        | MissingStaticInitializer of AssemblyId * int option
    with
        member r.Id = match r with | Loaded(id,_) | LoadFault(id,_) | MissingAssemblyImage id | MissingStaticInitializer(id,_) -> id
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
            | Some(dynamicAssemblyName, sliceId) ->
                let assemblyName = new AssemblyName(sliceName)
                do assemblyName.Name <- dynamicAssemblyName
                match s.DynamicAssemblies.TryFind assemblyName.FullName with
                | None -> None
                | Some info -> info.GeneratedSlices.TryFind sliceId |> Option.map (fun slice -> info, slice)
            | None -> None