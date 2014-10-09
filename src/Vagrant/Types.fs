namespace Nessos.Vagrant

    open System
    open System.Reflection

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

    /// static initialization data for portable assembly
    [<NoEquality; NoComparison>]
    type StaticInitializer =
        {
            /// Generation of given static initializer
            Generation : int

            /// Static initialization data
            Data : byte []

            /// Is partial static initialization data
            IsPartial : bool
        }

    /// Contains information necessary for the exportation of an assembly
    [<NoEquality; NoComparison>] 
    type PortableAssembly =
        {
            // Assembly Metadata
            Id : AssemblyId

            /// Raw image of the assembly
            Image : byte [] option

            /// Symbols file
            Symbols : byte [] option

            /// Static initialization data
            StaticInitializer : StaticInitializer option
        }
    with
        member pa.FullName = pa.Id.FullName
        member pa.GetName() = pa.Id.GetName()
        override id.ToString() = id.FullName

    /// Static initialization metadata
    [<NoEquality; NoComparison>] 
    type StaticInitializationInfo =
        {
            /// Generation of given static initializer
            Generation : int

            /// Is partial static initialization data
            IsPartial : bool

            /// Static initialization errors
            Errors : (FieldInfo * exn) []
        }

    /// Assembly load information
    type AssemblyLoadInfo =
        | NotLoaded of AssemblyId
        | LoadFault of AssemblyId * exn
        | Loaded of AssemblyId * isAppDomainLoaded:bool * staticInitialization:StaticInitializationInfo option
    with
        member info.Id = 
            match info with
            | NotLoaded id
            | LoadFault (id,_)
            | Loaded (id,_,_) -> id

    /// Exception raised by Vagrant
    type VagrantException (message : string, ?inner : exn) =
        inherit Exception(message, defaultArg inner null)