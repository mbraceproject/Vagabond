﻿module internal Nessos.Vagabond.AssemblyManagementTypes

open Nessos.Vagabond.SliceCompilerTypes
open Nessos.Vagabond.AssemblyCache

open Nessos.FsPickler
open Nessos.FsPickler.Hashing

/// Local dynamic assembly slice data dependency export state
type DataExportState =
    {
        /// Assembly path for slice
        AssemblyPath : string
        /// Last data dependency info
        Info : DataDependencyInfo
        /// Hashcode or serialization exception for last exported value
        Hash : Choice<HashResult, exn>
        /// Data persist file for dependency
        PersistFile : (DataDependencyId * string) option
    }

/// Immutable Vagabond state object
type VagabondState =
    {
        /// Dynamic assembly compiler state
        CompilerState : DynamicAssemblyCompilerState
        /// Locally compiled dynamic assembly data export state
        DataExportState : Map<string, DataExportState []>
        /// Local data import state
        DataImportState : Map<string, DataGeneration []>
        /// Assembly export state
        AssemblyExportState : Map<AssemblyId, VagabondAssembly>
        /// Assembly import state
        AssemblyImportState : Map<AssemblyId, AssemblyLoadInfo>
        /// Vagabond Serializer instance
        Serializer : FsPicklerSerializer
        /// Assembly Cache instance
        AssemblyCache : AssemblyCache
        /// Native assembly manager
        NativeAssemblyManager : NativeAssemblyManager
    }