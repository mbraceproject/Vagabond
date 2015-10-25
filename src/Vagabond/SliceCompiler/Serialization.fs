module internal Nessos.Vagabond.Serialization

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Reflection

open Nessos.FsPickler

open Nessos.Vagabond
open Nessos.Vagabond.SliceCompilerTypes

// FsPickler ITypeNameConverter binding for Vagabond
// Uses SliceCompiler state to determine serialization/deserialization mappings for slices.
// This is because types in the exporting process belong to the dynamic assembly,
// whereas types in the importing processes belong to static slices.
// An ITypeNameConverter implementation provides bridging when serializing/deserializing values
// on the exporting side.

type VagabondTypeNameConverter(getState : unit -> AssemblyCompilerState, ?compileNewSlice : Assembly -> unit) =

    interface ITypeNameConverter with
        // convert assembly qualified name dynamic -> slice
        member __.OfSerializedType(typeInfo : TypeInfo) =
            let qname = typeInfo.AssemblyInfo.AssemblyQualifiedName
            let rec remap isFirstAttempt =
                match getState().DynamicAssemblies.TryFind qname with
                | None -> typeInfo
                | Some info ->
                    // type belongs to a slice, detect what slice it belongs to and update accordingly
                    match info.TypeIndex.TryFind typeInfo.Name with
                    | None when isFirstAttempt && Option.isSome compileNewSlice -> 
                        // if type has not yet been compiled to slice, force compilation now and retry type mapping
                        compileNewSlice.Value info.DynamicAssembly ; remap false
                    | None | Some (InNoSlice | InAllSlices) ->
                        raise <| new VagabondException(sprintf "type '%s' in dynamic assembly '%s' does not correspond to slice." typeInfo.Name qname)

                    | Some (InSpecificSlice slice) -> 
                        { typeInfo with AssemblyInfo = { typeInfo.AssemblyInfo with Name = slice.Assembly.GetName().Name } }

            remap true
        
        // convert assembly qualified name slice -> dynamic
        member __.ToDeserializedType(typeInfo : TypeInfo) =
            match getState().TryGetDynamicAssemblyId typeInfo.AssemblyInfo.AssemblyQualifiedName with
            | None -> typeInfo
            | Some (assemblyName,_) -> { typeInfo with AssemblyInfo = { typeInfo.AssemblyInfo with Name = assemblyName } }

/// <summary>
/// Creates a typenameconverter passing a wrapped, user supplied type name converter instance
/// </summary>
/// <param name="forceLocalFSharpCore">Force local FSharp.Core assembly version when deserializing.</param>
/// <param name="wrappedConverter">User-supplied type converter.</param>
/// <param name="getState">Slice compiler state reader.</param>
/// <param name="compileNewSlice">Callback requesting slice compilation for given dynamic assembly by the serializer.</param>
let mkTypeNameConverter (forceLocalFSharpCore : bool) (wrappedConverter : ITypeNameConverter option) 
                        (getState : unit -> AssemblyCompilerState)
                        (compileNewSlice : (Assembly -> unit) option) =

        [|  yield new VagabondTypeNameConverter(getState, ?compileNewSlice = compileNewSlice) :> ITypeNameConverter
            if forceLocalFSharpCore then yield new LocalFSharpCoreConverter() :> _
            match wrappedConverter with Some w -> yield w | None -> () |]
        |> TypeNameConverter.compose