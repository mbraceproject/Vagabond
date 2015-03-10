namespace Nessos.Vagabond

open System
open System.IO
open System.Reflection
open System.Runtime.Serialization

open Nessos.FsPickler

/// Specifies what assemblies are to be loaded 
/// locally by the runtime if possible.
type AssemblyLoadPolicy =
    | None = 0
    /// Only signed assemblies should be looked up by runtime
    | ResolveStrongNames = 1
    /// All assembly names can be looked up by runtime
    | ResolveAll = 2
    /// If assembly is to be resolved locally, then it should have identical SHA256 hashcode.
    | RequireIdentical = 4
    /// Assemblies are to be cached only, not loaded in AppDomain
    | CacheOnly = 8
            

/// Vagabond unique assembly identifier
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
    /// Returns a System.Reflection.AssemblyName corresponding to Assembly id
    member id.GetName() = new AssemblyName(id.FullName)
    /// Is signed assembly
    member id.IsStrongAssembly = let pkt = id.GetName().GetPublicKeyToken() in pkt <> null && pkt <> [||]

    override id.ToString() = id.FullName

/// Vagabond metadata for dynamic assembly slices
[<NoEquality; NoComparison>]
type VagabondMetadata =
    {
        /// Generation of given static initializer
        Generation : int

        /// Is partial static initialization data
        IsPartial : bool

        /// Static initialization errors
        Errors : Pickle<FieldInfo * exn> []
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
        Metadata : (VagabondMetadata * string) option
    }
with
    /// Assembly qualified name
    member va.FullName = va.Id.FullName
    /// Returns a System.Reflection.AssemblyName corresponding to Assembly id
    member va.GetName() = va.Id.GetName()
    override id.ToString() = id.FullName


/// Assembly load information
type AssemblyLoadInfo =
    | NotLoaded of AssemblyId
    | LoadFault of AssemblyId * exn
    | Loaded of AssemblyId * isAppDomainLoaded:bool * metadata:VagabondMetadata option
with
    member info.Id = 
        match info with
        | NotLoaded id
        | LoadFault (id,_)
        | Loaded (id,_,_) -> id

/// Abstract assembly image exporting API
type IAssemblyExporter =
    /// Asynchronously returns a write stream for assembly image of given id.
    abstract GetImageWriter : id:AssemblyId -> Async<Stream>
    /// Asynchronously returns a write stream for assembly debug symbols of given id.
    abstract GetSymbolWriter : id:AssemblyId -> Async<Stream>
    /// Asynchronously returns a write stream for Vagabond data of given id.
    abstract WriteMetadata : id:AssemblyId * metadata:VagabondMetadata -> Async<Stream>

/// Abstract assembly image importing API
type IAssemblyImporter =
    /// Asynchronously returns a read stream for assembly image of given id.
    abstract GetImageReader : id:AssemblyId -> Async<Stream>
    /// Asynchronously returns a read stream for assembly debug symbols of given id.
    abstract TryGetSymbolReader : id:AssemblyId -> Async<Stream option>
    /// Asynchronously reads Vagabond metadata information for assembly of given id.
    abstract TryReadMetadata : id:AssemblyId -> Async<VagabondMetadata option>
    /// Asynchronously returns a read stream for Vagabond data of given id.
    abstract GetDataReader : id:AssemblyId * VagabondMetadata -> Async<Stream>

/// Exception raised by Vagabond
[<AutoSerializable(true)>] 
type VagabondException = 
    inherit Exception
    internal new (message : string, ?inner : exn) = { inherit Exception(message, defaultArg inner null) }
    private new (sI : SerializationInfo, sc : StreamingContext) =  { inherit Exception(sI, sc) }

[<AutoSerializable(true)>] 
type OutOfResourcesException =
    inherit Exception
    internal new (message : string) = { inherit Exception(message) }
    private new (sI : SerializationInfo, sc : StreamingContext) =  { inherit Exception(sI, sc) }