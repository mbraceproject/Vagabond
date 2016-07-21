module internal MBrace.Vagabond.AssemblyManagementTypes

open System.Reflection

open MBrace.Vagabond.SliceCompilerTypes
open MBrace.Vagabond.AssemblyCache

open MBrace.FsPickler
open MBrace.FsPickler.Hashing

/// Local dynamic assembly slice data dependency export state
type DataExportState =
    {
        /// Assembly path for slice
        AssemblyPath : string
        /// Data FieldInfo
        FieldInfo : FieldInfo
        /// Last data dependency info
        Info : DataDependencyInfo
        /// Hashcode or serialization exception for last exported value
        Hash : Choice<HashResult, exn>
        /// Data persist file for dependency
        PersistFile : (DataDependencyId * string) option
    }

type AssemblyLoadState =
    /// Assembly loaded from ambient domain
    | LoadedAssembly of VagabondAssembly
    /// Assembly slice exported from local process
    | ExportedSlice of DataExportState []
    /// Static assembly imported via Vagabond
    | ImportedAssembly of VagabondAssembly
    /// Dynamic Assembly slice imported via Vagabond
    | ImportedSlice of VagabondAssembly

/// Immutable Vagabond state object
type VagabondState =
    {
        /// User-supplied vagabond configuration object
        Configuration : VagabondConfiguration
        /// Dynamic assembly compiler state
        CompilerState : AssemblyCompilerState
        /// Vagabond Assembly load state
        AssemblyLoadState : Map<AssemblyId, AssemblyLoadState>
        /// Vagabond Serializer instance
        Serializer : FsPicklerSerializer
        /// Assembly Cache instance
        AssemblyCache : AssemblyCache
        /// Native assembly manager
        NativeAssemblyManager : NativeAssemblyManager
        /// Static field bindings loaded in local AppDomain
        StaticBindings : (FieldInfo * HashResult) []
    }