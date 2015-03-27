/// Raw in-memory assembly export/load implementations
module Nessos.Vagabond.ExportableAssembly

open System
open System.IO

/// A Vagabond assembly with all data loaded in-memory 
/// for instant exportation.
type ExportableAssembly =
    {
        /// Assembly Identifier
        Id : AssemblyId
        /// Assembly binary image
        ImageRaw : byte []
        /// Assembly symbols
        SymbolsRaw : byte [] option
        /// Vagabond metadata * static initializers
        Metadata : VagabondMetadata
        /// Raw peristed data dependencies
        PersistedDataRaw : (DataDependencyId * byte []) []
    }

type VagabondManager with

    /// <summary>
    ///     Copies raw vagabond assembly data to an in-memory record.
    /// </summary>
    /// <param name="va">Input vagabond assembly.</param>
    member v.CreateRawAssembly(va : VagabondAssembly) =
        let imageBuf = new MemoryStream()
        let symbols = va.Symbols |> Option.map (fun _ -> new MemoryStream())
        let persisted = ref Map.empty<DataDependencyId, MemoryStream>

        let exporter =
            {
                new IAssemblyExporter with
                    member x.TryGetImageWriter(id: AssemblyId) = async {
                        return Some (imageBuf :> _)
                    }

                    member x.TryGetSymbolsWriter(id: AssemblyId) = async {
                        return symbols |> Option.map unbox
                    }

                    member x.TryGetMetadata(id: AssemblyId) = async {
                        return None
                    }

                    member x.GetPersistedDataDependencyReader(id: AssemblyId, dataDependency: DataDependencyInfo) = async {
                        let mem = new MemoryStream()
                        persisted := persisted.Value.Add(dataDependency.Id, mem)
                        return mem :> _
                    }
                    
                    member x.WriteMetadata(id: AssemblyId, metadata: VagabondMetadata) = async.Zero()
            }

        v.ExportAssemblies(exporter, [|va|]) |> Async.RunSync

        let persisted = persisted.Value |> Map.toSeq |> Seq.map (fun (id,m) -> id, m.ToArray()) |> Seq.toArray

        {
            Id = va.Id
            ImageRaw = imageBuf.ToArray()
            SymbolsRaw = symbols |> Option.map (fun s -> s.ToArray())
            Metadata = va.Metadata
            PersistedDataRaw = persisted
        }

    /// <summary>
    ///     Copies raw vagabond assembly data to in-memory records.
    /// </summary>
    /// <param name="vas">Input vagabond assemblies.</param>
    member v.CreateRawAssemblies(vas : seq<VagabondAssembly>) : ExportableAssembly list =
        vas |> Seq.map v.CreateRawAssembly |> Seq.toList

    /// <summary>
    ///     Import a raw assembly to cache.
    /// </summary>
    /// <param name="raw">raw assembly input.</param>
    member v.CacheRawAssembly(ea : ExportableAssembly) : VagabondAssembly =
        let persisted = ea.PersistedDataRaw |> Map.ofArray
        let importer =
            {
                new IAssemblyImporter with
                    member __.GetImageReader(id: AssemblyId) = async { return new MemoryStream(ea.ImageRaw) :> _ }
                    member __.TryGetSymbolReader _ = async { return ea.SymbolsRaw |> Option.map (fun s -> new MemoryStream(s) :> _) }
                    
                    member __.GetPersistedDataDependencyReader(id: AssemblyId, dataDependency: DataDependencyInfo) = async {
                        return new MemoryStream(persisted.[dataDependency.Id]) :> Stream
                    }
                    
                    member __.ReadMetadata(id: AssemblyId) = async { return ea.Metadata }
            }

        v.ImportAssemblies(importer, [ea.Id]) |> Async.RunSync |> List.head

    /// <summary>
    ///     Import raw assemblies to cache.
    /// </summary>
    /// <param name="ras">raw assembly inputs.</param>
    member v.CacheRawAssemblies(ras : seq<ExportableAssembly>) : VagabondAssembly list =
        ras |> Seq.map (fun ra -> v.CacheRawAssembly ra) |> Seq.toList