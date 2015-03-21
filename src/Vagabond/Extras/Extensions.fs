namespace Nessos.Vagabond

[<AutoOpen>]
module Extensions =

    type AssemblyId with
        /// Generates a unique filename for supplied AssemblyId
        member id.FileName =
            let name = id.GetName().Name |> stripInvalidFileChars
            let hash = Convert.toBase32String id.ImageHash
            sprintf "%s-%s.dll" name hash