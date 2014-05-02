namespace Nessos.Vagrant

    open System
    open System.Reflection

    open Nessos.FsPickler

    /// unique identifier for assembly
    [<StructuralComparison>]
    [<StructuralEquality>]
    type AssemblyId =
        {
            /// assembly qualified name
            FullName : string
            /// digest of the raw assembly image
            ImageHash : byte []
        }
    with
        member id.GetName() = new AssemblyName(id.FullName)


    and [<NoEquality; NoComparison>] DynamicAssemblySliceInfo = 
        {
            SourceId : Guid
            DynamicAssemblyQualifiedName : string
            SliceId : int

            RequiresStaticInitialization : bool
            StaticInitializerGeneration : int
            IsPartiallyEvaluatedStaticInitializer : bool
        }

    /// Vagrant metadata on assembly
    and [<NoEquality; NoComparison>] AssemblyInfo =
        {
            /// Assembly Identifier
            Id : AssemblyId

            IsImageLoaded : bool
            IsSymbolsLoaded : bool

            DynamicAssemblySliceInfo : DynamicAssemblySliceInfo option
//            StaticInitializerGeneration : int
//            
//            IsPartiallyEvaluatedStaticInitializer : bool
//            RequiresStaticInitialization : bool
        }
    with
        member __.FullName = __.Id.FullName
        member __.IsDynamicAssemblySlice = __.DynamicAssemblySliceInfo.IsSome

    /// Contains information necessary for the exportation of an assembly
    and [<NoEquality;NoComparison>] PortableAssembly =
        {
            // Assembly Metadata
            Info : AssemblyInfo

            /// Raw image of the assembly
            Image : byte [] option

            /// Symbols file
            Symbols : byte [] option

            /// Static initialization data
            StaticInitializer : (int * byte []) option
        }
    with
        member pa.FullName = pa.Info.Id.FullName
        member pa.Id = pa.Info.Id
        member pa.GetName() = pa.Info.Id.GetName()
        

//    and [<NoEquality;NoComparison>] DynamicAssemblyInfo =
//        {
//            /// Unique identifier of dynamic assembly source
//            SourceId : Guid
//
//            /// Original dynamic assembly qualified name
//            DynamicAssemblyName : string
//
//            /// Identifier of current slice
//            SliceId : int
//
//            /// Slice contains static fields that require initialization
//            RequiresStaticInitialization : bool
//
//            /// Dynamic assembly requires further evaluation ; 
//            /// will require static initialization revision in the future.
//            IsPartiallyEvaluated : bool
//
//            /// Static initialization data
//            StaticInitializerData : (int * byte []) option
//        }

//    // Response given by the Vagrant client upon loading a PortableAssembly
//
    and AssemblyLoadResponse =
        | LoadSuccess of AssemblyId * (FieldInfo * exn) []
        | LoadFault of AssemblyId * exn
//        | MissingAssemblyImage of AssemblyId
//        | MissingStaticInitializer of AssemblyId * int option
    with
        member r.Id = match r with | LoadSuccess(id,_) | LoadFault(id,_) -> id // | MissingAssemblyImage id | MissingStaticInitializer(id,_) -> id
        member r.StaticInitializationErrors = match r with | LoadSuccess(_,errors) -> errors | _ -> [||]
        static member StaticSuccess id = LoadSuccess(id, [||])


    type IRemoteAssemblyReceiver =
        abstract GetLoadedAssemblyInfo : AssemblyId list -> Async<AssemblyInfo list>
        abstract PushAssemblies : PortableAssembly list -> Async<AssemblyLoadResponse list>

    type IRemoteAssemblyPublisher =
        abstract GetRequiredAssemblyInfo : unit -> Async<AssemblyId list>
        abstract PullAssemblies : AssemblyInfo list -> Async<PortableAssembly list>

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


    type VagrantException (message : string, ?inner : exn) =
        inherit Exception(sprintf "Vagrant error: %s"  message, defaultArg inner null)