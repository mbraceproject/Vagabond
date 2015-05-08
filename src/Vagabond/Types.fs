namespace Nessos.Vagabond

open System
open System.IO
open System.Reflection
open System.Runtime.Serialization

open Nessos.FsPickler
open Nessos.FsPickler.Hashing

/// Specifies what assemblies are to be loaded 
/// locally by the runtime if possible.
type AssemblyLoadPolicy =
    | None = 0
    /// Only signed assemblies should be looked up by runtime
    | ResolveStrongNames = 1
    /// All assembly names can be looked up by runtime
    | ResolveAll = 2
    /// If assembly is to be resolved locally, then it should have identical image hash.
    | RequireIdentical = 4

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
    /// Is signed assembly
    member id.IsStrongAssembly = let pkt = id.GetName().GetPublicKeyToken() in pkt <> null && pkt <> [||]

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
type IAssemblyExporter =
    /// Asynchronously returns a write stream for assembly image of given id. Returns 'None' if image already exists.
    abstract TryGetImageWriter : id:AssemblyId -> Async<Stream option>
    /// Asynchronously returns a write stream for assembly debug symbols of given id. Returns 'None' if symbols already exist.
    abstract TryGetSymbolsWriter : id:AssemblyId -> Async<Stream option>
    /// Asynchronously returns currently stored Vagabond metadata for given id. Returns 'None' if it does not exist.
    abstract TryGetMetadata : id:AssemblyId -> Async<VagabondMetadata option>
    /// Asynchronously returns a write stream for writing supplied persisted data dependency.
    abstract GetPersistedDataDependencyReader : id:AssemblyId * dataDependency:DataDependencyInfo -> Async<Stream>
    /// Asynchronously writes Vagabond metadata for assembly of provided id.
    abstract WriteMetadata : id:AssemblyId * metadata:VagabondMetadata -> Async<unit>

/// Abstract assembly image importing API
type IAssemblyImporter =
    /// Asynchronously returns a read stream for assembly image of given id.
    abstract GetImageReader : id:AssemblyId -> Async<Stream>
    /// Asynchronously returns a read stream for assembly debug symbols of given id.
    abstract TryGetSymbolReader : id:AssemblyId -> Async<Stream option>
    /// Asynchronously reads Vagabond metadata information for assembly of given id.
    abstract ReadMetadata : id:AssemblyId -> Async<VagabondMetadata>
    /// Asynchronously returns a read stream for Vagabond data of given id.
    abstract GetPersistedDataDependencyReader : id:AssemblyId * dataDependency:DataDependencyInfo -> Async<Stream>

/// Exception raised by Vagabond
[<AutoSerializable(true)>] 
type VagabondException = 
    inherit Exception
    internal new (message : string, ?inner : exn) = { inherit Exception(message, defaultArg inner null) }
    private new (sI : SerializationInfo, sc : StreamingContext) =  { inherit Exception(sI, sc) }