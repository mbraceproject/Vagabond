namespace MBrace.Vagabond

open System.IO
open System.IO.Compression

/// Abstract stream compression algorithm
type ICompressionAlgorithm =
    /// Compresses a stream
    abstract Compress : Stream -> Stream
    /// Decompresses a compressed stream
    abstract Decompress : Stream -> Stream

/// No Compression
type NoCompression() =
    interface ICompressionAlgorithm with
        member __.Compress(stream) = stream
        member __.Decompress(stream) = stream

/// GZip Compression algorithm
type GzipCompression(?compressionLevel : CompressionLevel) =
    let compressionLevel = defaultArg compressionLevel CompressionLevel.Fastest
    interface ICompressionAlgorithm with
        member __.Compress(stream:Stream) = new GZipStream(stream, compressionLevel) :> _
        member __.Decompress(stream:Stream) = new GZipStream(stream, CompressionMode.Decompress) :> _