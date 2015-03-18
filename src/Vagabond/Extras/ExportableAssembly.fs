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
        MetadataRaw : (VagabondMetadata * byte []) option
    }

type VagabondManager with

    /// <summary>
    ///     Copies raw vagabond assembly data to an in-memory record.
    /// </summary>
    /// <param name="va">Input vagabond assembly.</param>
    member v.CreateRawAssembly(va : VagabondAssembly) =
        let imageBuf = new MemoryStream()
        let symbols = va.Symbols |> Option.map (fun _ -> new MemoryStream())
        let initializers = va.Metadata |> Option.map (fun (m,_) -> m, new MemoryStream())
        let exporter =
            {
                new IAssemblyExporter with
                    member __.GetImageWriter _ = async { return imageBuf :> _ }
                    member __.GetSymbolWriter _ = async { return Option.get symbols :> _ }
                    member __.WriteMetadata (_,_) = async { return initializers |> Option.get |> snd :> _ }
            }

        v.ExportAssemblies(exporter, [|va|]) |> Async.RunSync

        {
            Id = va.Id
            ImageRaw = imageBuf.ToArray()
            SymbolsRaw = symbols |> Option.map (fun s -> s.ToArray())
            MetadataRaw = initializers |> Option.map (fun (m,s) -> m, s.ToArray())
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
    member v.CacheRawAssembly(ra : ExportableAssembly) : VagabondAssembly =
        let importer =
            {
                new IAssemblyImporter with
                    member __.GetImageReader _ = async { return new MemoryStream(ra.ImageRaw) :> _ }
                    member __.TryGetSymbolReader _ = async { return ra.SymbolsRaw |> Option.map (fun s -> new MemoryStream(s) :> _) }
                    member __.TryReadMetadata _ = async { return ra.MetadataRaw |> Option.map fst }
                    member __.GetDataReader (_,_) = async { return let _,d = ra.MetadataRaw |> Option.get in new MemoryStream(d) :> _ }
            }

        v.ImportAssemblies(importer, [ra.Id]) |> Async.RunSync |> List.head

    /// <summary>
    ///     Import raw assemblies to cache.
    /// </summary>
    /// <param name="ras">raw assembly inputs.</param>
    member v.CacheRawAssemblies(ras : seq<ExportableAssembly>) : VagabondAssembly list =
        ras |> Seq.map (fun ra -> v.CacheRawAssembly ra) |> Seq.toList