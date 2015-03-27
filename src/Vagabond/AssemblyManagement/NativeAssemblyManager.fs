namespace Nessos.Vagabond

open System
open System.Collections.Generic
open System.IO

open Nessos.Vagabond.AssemblyNaming

/// AppDomain-specific native assembly container
/// container is added to local PATH and copied
/// from cache whenever loaded
type internal NativeAssemblyManager(path : string) =
    let uuid = Guid.NewGuid().ToByteArray().[..7] |> Convert.toBase32String
    let container = Path.Combine(path, "native-" + uuid)
    let installed = new Dictionary<AssemblyId, VagabondAssembly> ()
    do 
        let _ = Directory.CreateDirectory container
        // add cache path to environment for loading unmanaged assemblies
        let envPath = Environment.GetEnvironmentVariable("PATH")
        if envPath.Contains (";" + container) then ()
        else Environment.SetEnvironmentVariable("PATH", envPath + ";" + container)

    /// Loads a native assembly by copying to the native path
    member __.Load(va : VagabondAssembly) =
        try
            let cachePath = Path.Combine(container, Path.ChangeExtension(va.Id.FullName, va.Id.Extension))
            let shouldCopy =
                if File.Exists cachePath then
                    let cachedId = AssemblyIdGenerator.GetManagedAssemblyId cachePath
                    cachedId <> va.Id
                else
                    true

            if shouldCopy then File.Copy(va.Image, cachePath, true)
            // add copied assembly to state
            installed.Add(va.Id, { va with Image = cachePath })

            Loaded(va.Id, true, va.Metadata)

        with e -> LoadFault(va.Id, e)

    /// Try get assembly that is loaded in native assembly state
    member __.TryFind(id : AssemblyId) =
        let ok, found = installed.TryGetValue id
        if ok then Some found
        else None

    /// Gets loaded native assemblies
    member __.LoadedNativeAssemblies = installed |> Seq.map (function KeyValue(_,v) -> v) |> Seq.toList