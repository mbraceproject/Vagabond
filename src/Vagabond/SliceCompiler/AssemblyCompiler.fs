module internal MBrace.Vagabond.SliceCompiler

open System
open System.IO
open System.Collections.Generic
open System.Reflection

open Mono.Cecil

open MBrace.FsPickler

open MBrace.Vagabond.Utils
open MBrace.Vagabond.SliceCompilerTypes
open MBrace.Vagabond.AssemblyNaming
open MBrace.Vagabond.AssemblyParser

/// creates a path for provided assembly name so that
/// invalid characters are stripped and overwrites are avoided.
let getAssemblyPath (path : string) (name : string) =
    let stripped = stripInvalidFileChars name
    let rec getSuffix i =
        let path = 
            if i = 0 then Path.Combine(path, stripped + ".dll")
            else Path.Combine(path, sprintf "%s-%d.dll" name i)

        if File.Exists path then getSuffix (i+1)
        else path

    getSuffix 0 


/// create an initial, empty compiler state
let initCompilerState (uuid : Guid) (profiles : IDynamicAssemblyProfile []) (outDirectory : string) =
    {
        CompilerId = uuid
        Profiles = profiles
        OutputDirectory = outDirectory

        DynamicAssemblies = Map.empty
        InMemoryAssemblies = Map.empty

        TryGetDynamicAssemblyId = AssemblySliceName.tryParseLocalDynamicAssemblySlice uuid
        CreateAssemblySliceName = AssemblySliceName.mkSliceName uuid
    }

/// compiles an in-memory assembly to disk
let compileInMemoryAssembly (state : AssemblyCompilerState) (origin : Assembly) =
    let parsedAssembly = parseInMemoryAssembly state origin
    let name = origin.GetName().Name
    let target = getAssemblyPath state.OutputDirectory name
    // write parsed assebmly to disk
    do parsedAssembly.Write(target)

    // load new slice to System.Reflection
    // Assembly.LoadFrom() replaces the deprecated Assembly.ReflectionOnlyLoadFrom() call
    // TODO: migrate to System.Reflection.Metadata
    let assembly = Utils.currentLoadContext.LoadFromAssemblyPath(target)

    let assemblyInfo = 
        { 
            Origin = origin
            CompiledAssembly = assembly
        }

    let inMemoryAssemblyIndex = state.InMemoryAssemblies.Add(origin.FullName, assemblyInfo)
    let state = { state with InMemoryAssemblies = inMemoryAssemblyIndex }
    assemblyInfo, state


/// compiles a slice of given dynamic assembly snapshot
let compileDynamicAssemblySlice (state : AssemblyCompilerState) (dynamicAssembly : Assembly) =

    let typeData, assemblyState, parsedSlice = parseDynamicAssemblySlice state dynamicAssembly

    // prepare slice info
    let sliceId = assemblyState.GeneratedSlices.Count + 1
    let name = state.CreateAssemblySliceName assemblyState.Name.Name sliceId
    let target = getAssemblyPath state.OutputDirectory name

    // update assembly name & write to disk
    do parsedSlice.Name.Name <- name
    do parsedSlice.Write(target)

    // load new slice to System.Reflection
    // Assembly.LoadFrom() replaces the deprecated Assembly.ReflectionOnlyLoadFrom() call
    // TODO: migrate to System.Reflection.Metadata
    let assembly = currentLoadContext.LoadFromAssemblyPath(target)
        
    // collect pickleable static fields
    let pickleableFields = 
        typeData 
        |> Seq.map (function KeyValue(_,InCurrentSlice(_,fields)) -> fields | _ -> [||] ) 
        |> Array.concat

    let sliceInfo = 
        { 
            Assembly = assembly 
            DynamicAssemblyQualifiedName = assemblyState.DynamicAssembly.FullName 
            SliceId = sliceId 
            StaticFields = pickleableFields
        }

    // update generated slices
    let generatedSlices = assemblyState.GeneratedSlices.Add(sliceId, sliceInfo)
        
    // update the type index
    let mapTypeIndex (_ : string) (info : TypeParseInfo) =
        match info with
        | AlwaysIncluded -> InAllSlices
        | InCurrentSlice _ -> InSpecificSlice sliceInfo
        | InPastSlice (slice = slice) -> InSpecificSlice slice
        | Erased -> InNoSlice

    let typeIndex = typeData |> Map.map mapTypeIndex

    // return state
    let assemblyState = { assemblyState with GeneratedSlices = generatedSlices ; TypeIndex = typeIndex }
    let dynamicAssemblyIndex = state.DynamicAssemblies.Add(assemblyState.DynamicAssembly.FullName, assemblyState)
    let state = { state with DynamicAssemblies = dynamicAssemblyIndex}

    sliceInfo, state

/// compiles a single assembly
let compileAssembly (state : AssemblyCompilerState) (assembly : Assembly) : Assembly * AssemblyCompilerState =
    match assembly with
    | StaticAssembly _ -> raise <| new VagabondException(sprintf "internal error: attempting to compile static assembly %A" assembly)
    | DynamicAssembly -> 
        let result, state = compileDynamicAssemblySlice state assembly
        result.Assembly, state
    | InMemoryAssembly ->
        let result, state = compileInMemoryAssembly state assembly
        result.CompiledAssembly, state

/// compiles a collection of assemblies
let compileAssemblies (state : AssemblyCompilerState) (assemblies : Assembly list) =
    // exceptions are handled explicitly so that returned state reflects the last successful compilation
    let aux ((state : AssemblyCompilerState, result : Exn<Assembly list>) as s) (assembly : Assembly) =
        match result with
        | Success compiled ->
            try
                let result, state = compileAssembly state assembly
                state, Success(result :: compiled)
            with e ->
                state, Error e

        | Error _ -> s

    List.fold aux (state, Success []) assemblies