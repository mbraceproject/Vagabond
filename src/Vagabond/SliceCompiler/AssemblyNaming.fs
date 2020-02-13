module internal MBrace.Vagabond.AssemblyNaming

open System
open System.IO
open System.Security.Cryptography
open System.Reflection
open System.Text.RegularExpressions
open System.Threading

open MBrace.FsPickler.Hashing
open MBrace.Vagabond

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
        match assembly with
        | DynamicAssembly ->
            // generating Assembly id's for dynamic assemblies is convenient,
            // but such instances should not leak to the public API.
            // generate a few random bytes for 'hash'.
            // this is ok since we only access the API through memoization
            let hash = Guid.NewGuid().ToByteArray()
            let extension = "__dynamic__assembly__"
            { FullName = assembly.FullName ; ImageHash = hash ; Extension = extension }

        | InMemoryAssembly ->
            let hash = Guid.NewGuid().ToByteArray()
            let extension = "__inmemory__assembly__"
            { FullName = assembly.FullName ; ImageHash = hash ; Extension = extension }

        | StaticAssembly location ->
            let hash = computeHash location
            let extension = Path.GetExtension location
            { FullName = assembly.FullName ; ImageHash = hash ; Extension = extension }

    static let getMemoizedManagedAssemblyId = concurrentMemoize getManagedAssemblyId

    /// Computes the assembly id for provided managed assembly.
    static member GetManagedAssemblyId(assembly : Assembly) = getMemoizedManagedAssemblyId assembly

    /// Unmemoized, unmanaged assembly id generator
    static member GetAssemblyId(path : string) =
        let name = Path.GetFileNameWithoutExtension path
        let extension = Path.GetExtension path
        let hash = computeHash path
        { FullName = name ; ImageHash = hash ; Extension = extension }


module AssemblySliceName =
    
    /// Generates a slice name using given uuid, name and slice id.
    let mkSliceName (uuid : Guid) (name : string) (sliceId : int) = sprintf "%s_%O_%d" name uuid sliceId

    /// try parsing a dynamic assembly slice
    let tryParseDynamicAssemblySlice =
        let guidRegex = "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
        let sliceRegex = new Regex("^(.*)_(" + guidRegex + ")_([0-9]+)", RegexOptions.Compiled)
        fun (assemblyName : string) ->
            let m = sliceRegex.Match assemblyName
            if m.Success then
                let dynamicName = m.Groups.[1].Value
                let uuid = Guid.Parse m.Groups.[2].Value
                let sliceId = int <| m.Groups.[3].Value
                Some(uuid, dynamicName, sliceId)
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

    static member OfFullName(fullName:string) =
        {
            FullName = fullName
            ImageHash = [||]
            Extension = ""
        }

    /// Is Strong Assembly Name
    member id.IsStrongAssembly = id.GetName().IsStrongAssembly

    /// checks if provided assembly can be resolved from local environment
    /// based on the provided load policy
    member id.CanBeResolvedLocally (policy : AssemblyLookupPolicy) =
        if policy.HasFlag AssemblyLookupPolicy.ResolveRuntimeStrongNames then
            id.IsStrongAssembly
        else 
            policy.HasFlag AssemblyLookupPolicy.ResolveRuntime
        
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
        | Some(guid, assemblyName, sliceId) -> 
            // we cache using a shortened vagabond guid; 
            // we do this since uniqueness is guaranteed by the hash alone.
            // Guid is added simply to differentiate assemblies coming from different sources.
            let g = guid.ToString("N")
            sprintf "%s-%O-%s-%d-%s" (truncate 30 assemblyName) an.Version (truncate 5 g) sliceId hash
        // strip invalid characters; fix scriptcs bug
        |> stripInvalidFileChars


type DataDependency private () =
    // strips A.B.C. namespace prefixes in type names
    static let stripNamespaceRegex = new Regex(@"([^\.,\[]+\.)+", RegexOptions.Compiled)
    static let stripNamespacesFromTypeName (typeName : string) = stripNamespaceRegex.Replace(typeName, "")

    static let truncate (n : int) (txt : string) =
        if n >= txt.Length then txt
        else txt.Substring(0, n)

    // encode a positive long to variable-length byte array
    // we do this to save a few bytes and avoid the dreaded PathTooLongException.
    static let long2Bytes (long : int64) =
        let bs = new ResizeArray<byte> ()
        let mutable long = long
        while long > 0L do
            bs.Add (byte long)
            long <- long / 256L
        bs.ToArray()

    /// Creates a unique filename for given hashcode
    static member CreateUniqueFileNameByHash(hash : HashResult, ?prefixId : AssemblyId) =
        let lengthEnc = long2Bytes hash.Length |> Convert.toBase32String
        let hashEnc = hash.Hash |> Convert.toBase32String
        let typeName = hash.Type |> stripNamespacesFromTypeName |> truncate 40
        match prefixId with
        | Some id ->
            let guid,_,_ = AssemblySliceName.tryParseDynamicAssemblySlice id.FullName |> Option.get
            let b32 = guid.ToByteArray() |> Convert.toBase32String |> truncate 7
            sprintf "%s-%s-%s-%s" b32 typeName lengthEnc hashEnc

        | None ->
            sprintf "%s-%s-%s" typeName lengthEnc hashEnc
        // strip invalid characters; fix scriptcs bug
        |> stripInvalidFileChars

type VagabondAssembly with
    /// Defines an unmanaged VagabondAsembly for provided file
    static member FromNativeAssembly(path : string) =
        let id = AssemblyIdGenerator.GetAssemblyId path
        let fileName = Path.GetFileName path
        let metadata = 
            { IsNativeAssembly = true ; OriginalFileName = fileName ; ProcessorArchitecture = ProcessorArchitecture.None ; 
                IsDynamicAssemblySlice = false ; DataDependencies = [||] }

        { Id = id ; Image = path ; Symbols = None ; Metadata = metadata ; PersistedDataDependencies = [||] }

    /// Defines a VagabondAssembly for provided managed assembly
    static member FromManagedAssembly(assembly : Assembly, isDynamicAssemblySlice : bool, dataDependencies, dataFiles) =
        match assembly with
        | StaticAssembly location ->
            let symbols =
                let file = getSymbolsPath location
                if File.Exists file then Some file else None

            let id = assembly.AssemblyId

            let metadata = 
                { 
                    IsNativeAssembly = false
                    ProcessorArchitecture = assembly.GetName().ProcessorArchitecture
                    OriginalFileName = Path.GetFileName assembly.Location
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

        | a -> raise <| new VagabondException(sprintf "Internal error: attempting to export non-static assembly %A" a)