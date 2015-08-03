namespace Nessos.Vagabond

open System
open System.IO
open System.Reflection
open System.Runtime.Serialization

open Nessos.FsPickler
open Nessos.FsPickler.Hashing

/// Specifies how assemblies are to be
/// looked up from the local vagabond context.
type AssemblyLookupPolicy =
    /// No lookup policy.
    | None = 0
    /// Resolve assemblies from the Vagabond cache directory.
    | ResolveVagabondCache = 1
    /// Resolve assemblies from the CLR context.
    | ResolveRuntime = 2
    /// Resolve assemblies that carry strong names from the CLR context.
    | ResolveRuntimeStrongNames = 4
    /// Require CLR loaded assemblies to carry identical hash.
    | RuntimeResolutionRequireIdenticalHash = 8
    /// Require all assembly dependencies of local objects to
    /// be loadable in application domain.
    | RequireLocalDependenciesLoadedInAppDomain = 16

/// Vagabond unique assembly identifier
[<StructuralComparison>]
[<StructuralEquality>]
type AssemblyId =
    {
        /// assembly qualified name
        FullName : string
        /// digest of the raw assembly image
        ImageHash : byte []
        /// Specifies the filename extension.
        Extension : string
    }
with
    /// Returns a System.Reflection.AssemblyName corresponding to Assembly id
    member id.GetName() = new AssemblyName(id.FullName)

    override id.ToString() = id.FullName


/// Data dependency identifier
type DataDependencyId = int
/// Pickle generation id for data dependency
type DataGeneration = int

/// Specifies data dependency content
type DataDependency =
    /// Data dependency failed to serialize
    | Errored of message:string * Pickle<exn>
    /// Data dependency pickled in-memory
    | Pickled of hash:HashResult * pickle:Pickle<obj>
    /// Data dependency pickled to file; reserved for large data bindings
    | Persisted of hash:HashResult

/// Data dependency information
type DataDependencyInfo =
    { 
        /// Unique identifier
        Id : DataDependencyId
        /// Human-readable dependency identifier.
        Name : string
        /// Data dependency generation.
        Generation : DataGeneration
        /// Pickled static field from which data was extracted.
        FieldInfo : Pickle<FieldInfo>
        /// Data dependency container.
        Data : DataDependency
    }

/// Vagabond metadata for dynamic assembly slices
[<NoEquality; NoComparison>]
type VagabondMetadata =
    {
        /// Specifies if is managed CIL assembly.
        IsManagedAssembly : bool

        /// Specifies if assembly is dynamic assembly slice.
        IsDynamicAssemblySlice : bool

        /// Static data dependencies for assembly; 
        /// used in dynamic assembly slices with erased static constructors.
        DataDependencies : DataDependencyInfo []
    }

/// Exportable Vagabond assembly and metadata
[<NoEquality; NoComparison>] 
type VagabondAssembly =
    {
        /// Assembly Identifier
        Id : AssemblyId

        /// path to assembly
        Image : string

        /// path to symbols file
        Symbols : string option

        /// Vagabond metadata and static initialization data path
        Metadata : VagabondMetadata

        /// Paths to bindings that are persisted to disk
        PersistedDataDependencies : (DataDependencyId * string) []
    }
with
    /// Assembly qualified name
    member va.FullName = va.Id.FullName
    /// Returns a System.Reflection.AssemblyName corresponding to Assembly id
    member va.GetName() = va.Id.GetName()
    override id.ToString() = id.FullName


/// Assembly load information
type AssemblyLoadInfo =
    /// Assembly does not exist in remote party.
    | NotLoaded of AssemblyId
    /// Error when attempting to load assembly.
    | LoadFault of AssemblyId * exn
    /// Assembly successfuly loaded in remote party.
    | Loaded of AssemblyId * isAppDomainLoaded:bool * metadata:VagabondMetadata
with
    member info.Id = 
        match info with
        | NotLoaded id
        | LoadFault (id,_)
        | Loaded (id,_,_) -> id

/// Abstract assembly image exporting API
type IAssemblyUploader =
    /// Asynchronously returns a write stream for assembly image of given id. Returns 'None' if image already exists.
    abstract TryGetImageWriter : id:AssemblyId -> Async<Stream option>
    /// Asynchronously returns a write stream for assembly debug symbols of given id. Returns 'None' if symbols already exist.
    abstract TryGetSymbolsWriter : id:AssemblyId -> Async<Stream option>
    /// Asynchronously returns a write stream for writing supplied persisted data dependency. Returns 'None' if dependency already exists.
    abstract TryGetPersistedDataDependencyWriter : id:AssemblyId * info:DataDependencyInfo * hash:HashResult -> Async<Stream option>
    /// Asynchronously writes Vagabond metadata for assembly of provided id.
    abstract WriteMetadata : id:AssemblyId * metadata:VagabondMetadata -> Async<unit>

/// Abstract assembly image importing API
type IAssemblyDownloader =
    /// Asynchronously returns a read stream for assembly image of given id.
    abstract GetImageReader : id:AssemblyId -> Async<Stream>
    /// Asynchronously returns a read stream for assembly debug symbols of given id. Returns 'None' if symbols of given assembly do not exist.
    abstract TryGetSymbolReader : id:AssemblyId -> Async<Stream option>
    /// Asynchronously reads Vagabond metadata information for assembly of given id.
    abstract ReadMetadata : id:AssemblyId -> Async<VagabondMetadata>
    /// Asynchronously returns a read stream for Vagabond data of given id.
    abstract GetPersistedDataDependencyReader : id:AssemblyId * info:DataDependencyInfo * hash:HashResult -> Async<Stream>

/// Vagabond configuration record
type VagabondConfiguration =
    {
        /// Cache directory used by Vagabond.
        CacheDirectory : string
        /// Local assembly resolution policy.
        AssemblyLookupPolicy : AssemblyLookupPolicy
        /// Object size threshold in bytes after which data dependencies should be persisted to disk.
        DataPersistThreshold : int64
        /// Dynamic assembly profiles used by Vagabond
        DynamicAssemblyProfiles : IDynamicAssemblyProfile []
        /// FsPickler type name converter used by Vagabond serializer instance.
        TypeConverter : ITypeNameConverter option
        /// Predicate for ignored assemblies
        IsIgnoredAssembly : Assembly -> bool
        /// Data compression algorithm used by vagabond
        DataCompressionAlgorithm : ICompressionAlgorithm
    }

/// Exception raised by Vagabond
[<AutoSerializable(true)>] 
type VagabondException = 
    inherit Exception
    internal new (message : string, ?inner : exn) = { inherit Exception(message, defaultArg inner null) }
    private new (sI : SerializationInfo, sc : StreamingContext) =  { inherit Exception(sI, sc) }