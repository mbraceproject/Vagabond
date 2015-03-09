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

/// Contains information necessary for the exportation of an assembly
[<NoEquality; NoComparison>] 
type VagabondAssembly =
    {
        /// Assembly Identifier
        Id : AssemblyId

        /// Path to assembly
        Image : string

        /// Path to symbols file
        Symbols : string option

        /// Vagabond metadata and static initialization data path
        Metadata : (VagabondMetadata * string) option
    }
with
    member pa.FullName = pa.Id.FullName
    member pa.GetName() = pa.Id.GetName()
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


type IAssemblyExporter =
    abstract GetImageWriter : AssemblyId -> Async<Stream>
    abstract GetSymbolWriter : AssemblyId -> Async<Stream>
    abstract WriteMetadata : AssemblyId * VagabondMetadata -> Async<Stream>

type IAssemblyImporter =
    abstract GetImageReader : AssemblyId -> Async<Stream>
    abstract TryGetSymbolReader : AssemblyId -> Async<Stream option>
    abstract TryReadMetadata : AssemblyId -> Async<VagabondMetadata option>
    abstract GetDataReader : AssemblyId * VagabondMetadata -> Async<Stream>

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