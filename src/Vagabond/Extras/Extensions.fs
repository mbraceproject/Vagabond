namespace Nessos.Vagabond

[<AutoOpen>]
module Extensions =

    type AssemblyId with
        /// Gets a unique assebly filename for supplied AssemblyId
        member id.AssemblyFileName = id.GetAssemblyFileName()