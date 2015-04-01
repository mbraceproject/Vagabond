module internal Nessos.Vagabond.AssemblyNaming

open System
open System.IO
open System.Security.Cryptography
open System.Reflection
open System.Text.RegularExpressions
open System.Threading

open Nessos.Vagabond

/// computes a unique assembly identifier
type AssemblyIdGenerator private () =
    // AssemblyId's used to have SHA256 for hashing.
    // have moved to MD5 to reduce size to 128 bits.
    // this is not really a problem since there is no
    // requirement of cryptographic properties here.
    // MD5 instances are not thread safe, wrap to ThreadLocal<_>
    static let hashAlgorithm = new ThreadLocal<_>(fun () -> MD5.Create())

    /// computes a hash based on the MD5 + length of a provided file
    /// size should be at most 8 + 16 = 24 bytes or 48 base32 chars
    static let computeHash (path : string) =
        // encode a positive long to variable-length byte array
        // since MD5 hash is of fixed size, this is not a problem.
        // we do this to save a few bytes and avoid the dreaded PathTooLongException.
        let long2Bytes (long : int64) =
            let bs = new ResizeArray<byte> ()
            let mutable long = long
            while long > 0L do
                bs.Add (byte long)
                long <- long / 256L
            bs.ToArray()
        
        let fileInfo = new FileInfo(path)
        let bsize = long2Bytes fileInfo.Length
        let hash = use fs = File.OpenRead path in hashAlgorithm.Value.ComputeHash fs
        Array.append bsize hash

    static let getManagedAssemblyId(assembly : Assembly) =
        if assembly.IsDynamic then
            // generating Assembly id's for dynamic assemblies is convenient,
            // but such instances should not leak to the public API.
            // generate a few random bytes for 'hash'.
            // this is ok since we only access the API through memoization
            let hash = Guid.NewGuid().ToByteArray()
            let extension = "__dynamic__assembly__"
            { FullName = assembly.FullName ; ImageHash = hash ; Extension = extension }
        else
            let location = assembly.Location
            let hash = computeHash location
            let extension = Path.GetExtension location
            { FullName = assembly.FullName ; ImageHash = hash ; Extension = extension }

    static let getMemoizedManagedAssemblyId = concurrentMemoize getManagedAssemblyId

    /// Computes the assembly id for provided managed assembly.
    static member GetManagedAssemblyId(assembly : Assembly) = getMemoizedManagedAssemblyId assembly

    /// Unmemoized, unmanaged assembly id generator
    static member GetManagedAssemblyId(path : string) =
        let name = Path.GetFileName path
        let extension = Path.GetExtension path
        let hash = computeHash path
        { FullName = name ; ImageHash = hash ; Extension = extension }


module AssemblySliceName =
    
    /// Generates a slice name using given uuid, name and slice id.
    let mkSliceName (uuid : Guid) (name : string) (sliceId : int) = sprintf "%s_%O_%d" name uuid sliceId

    /// try parsing a dynamic assembly slice
    let tryParseDynamicAssemblySlice =
        let guidRegex = "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
        let sliceRegex = new Regex("^(.*)_(" + guidRegex + ")_([0-9]+)")
        fun (assemblyName : string) ->
            let m = sliceRegex.Match assemblyName
            if m.Success then
                let dynamicName = m.Groups.[1].Value
                let uuid = Guid.Parse <| m.Groups.[2].Value
                let sliceId = int <| m.Groups.[3].Value
                Some(dynamicName, sliceId, uuid)
            else
                None

    /// try parsing a dynamic assembly slice that corresponds to particular Guid
    let tryParseLocalDynamicAssemblySlice (uuid : Guid) =
        let sliceRegex = new Regex(sprintf "^(.*)_%O_([0-9]+)" uuid, RegexOptions.Compiled)
        fun (assemblyName : string) ->
            let m = sliceRegex.Match assemblyName
            if m.Success then
                // is slice of given uuid, parse name and slice id
                let dynamicName = m.Groups.[1].Value
                let sliceId = int <| m.Groups.[2].Value
                Some (dynamicName, sliceId)
            else
                None


type Assembly with
    /// Gets the Assembly Id for assembly instance
    member a.AssemblyId = AssemblyIdGenerator.GetManagedAssemblyId a

type AssemblyId with
    /// checks if provided assembly can be resolved from local environment
    /// based on the provided load policy
    member id.CanBeResolvedLocally (policy : AssemblyLoadPolicy) =
        if policy.HasFlag AssemblyLoadPolicy.ResolveAll then true
        elif policy.HasFlag AssemblyLoadPolicy.ResolveStrongNames then 
            id.IsStrongAssembly
        else
            false
        
    /// Gets a unique assembly file name based on provided assembly id
    member id.GetFileName() =
        let an = id.GetName()
        let hash = Convert.toBase32String id.ImageHash
        let truncate n (text : string) = 
            if text.Length <= n then text
            else text.Substring(0, n)

        // the dreaded PathTooLongException lurks, 
        // need to keep file names as short as possible
        // without losing uniqueness.
        match AssemblySliceName.tryParseDynamicAssemblySlice an.Name with
        | None -> sprintf "%s-%O-%s" (truncate 40 an.Name) an.Version hash
        | Some(name, slice, guid) -> 
            // we cache using a shortened vagabond guid; 
            // we do this since uniqueness is guaranteed by the hash alone.
            // Guid is added simply to differentiate assemblies coming from different sources.
            let g = guid.ToString("N")
            sprintf "%s-%O-%s-%d-%s" (truncate 30 name) an.Version (truncate 5 g) slice hash
        // strip invalid characters; fix scriptcs bug
        |> stripInvalidFileChars

type VagabondAssembly with
    /// Defines an unmanaged VagabondAsembly for provided file
    static member CreateUnmanaged(path : string) =
        let id = AssemblyIdGenerator.GetManagedAssemblyId path
        let metadata = { IsManagedAssembly = false ; IsDynamicAssemblySlice = false ; DataDependencies = [||] }
        { Id = id ; Image = path ; Symbols = None ; Metadata = metadata ; PersistedDataDependencies = [||] }

    /// Defines un unmanaged VagabondAssembly for provided managed assembly
    static member CreateManaged(assembly : Assembly, isDynamicAssemblySlice : bool, dataDependencies, dataFiles) =
        let location = assembly.Location
        let extension = Path.GetExtension location
        let symbols =
            let file = getSymbolsPath location
            if File.Exists file then Some file else None

        let id = assembly.AssemblyId
        let metadata = 
            { 
                IsManagedAssembly = true
                IsDynamicAssemblySlice = isDynamicAssemblySlice
                DataDependencies = dataDependencies
            }

        {
            Id = id
            Image = location
            Symbols = symbols
            Metadata = metadata
            PersistedDataDependencies = dataFiles
        }