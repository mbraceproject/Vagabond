module internal Nessos.Vagrant.SliceCompiler

    open System
    open System.IO
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open System.Reflection

    open Mono.Cecil

    open Nessos.FsPickler

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.SliceCompilerTypes
    open Nessos.Vagrant.Serialization
    open Nessos.Vagrant.AssemblyParser
    open Nessos.Vagrant.DependencyAnalysis

    /// create an initial, empty compiler state

    let initCompilerState (profiles : IDynamicAssemblyProfile list) (outDirectory : string) =
        let uuid = Guid.NewGuid()
        let mkSliceName (name : string) (id : int) = sprintf "%s_%O_%d" name uuid id
        let assemblyRegex = Regex(sprintf "^(.*)_%O_([0-9]+)" uuid)
        let tryExtractDynamicAssemblyId (assemblyName : string) =
            let m = assemblyRegex.Match(assemblyName)
            if m.Success then 
                let dynamicName = m.Groups.[1].Value
                let sliceId = int <| m.Groups.[2].Value
                Some (dynamicName, sliceId)
            else
                None
        {
            ServerId = Guid.NewGuid()
            Profiles = profiles
            OutputDirectory = outDirectory

            DynamicAssemblies = Map.empty

            TryGetDynamicAssemblyId = tryExtractDynamicAssemblyId
            CreateAssemblySliceName = mkSliceName
        }


    /// compiles a slice of given dynamic assembly snapshot

    let compileDynamicAssemblySlice (state : DynamicAssemblyCompilerState)
                                    (assemblyState : DynamicAssemblyState)
                                    (typeData : Map<string, TypeParseInfo>)
                                    (slice : AssemblyDefinition) =

        // prepare slice info
        let sliceId = assemblyState.GeneratedSlices.Count + 1
        let name = state.CreateAssemblySliceName assemblyState.Name.Name sliceId
        let target = System.IO.Path.Combine(state.OutputDirectory, name + ".dll")

        // update assembly name & write to disk
        do slice.Name.Name <- name
        do slice.Write(target)

        // load new slice to System.Reflection
        let assembly = Assembly.ReflectionOnlyLoadFrom(target)
        
        // collect pickleable static fields
        let pickleableFields = 
            typeData 
            |> Seq.map (function KeyValue(_,InCurrentSlice(_,fields)) -> fields | _ -> [||] ) 
            |> Array.concat

        let sliceInfo = 
            { 
                SourceId = state.ServerId
                Assembly = assembly 
                DynamicAssemblyQualifiedName = assemblyState.DynamicAssembly.FullName 
                SliceId = sliceId 
                StaticFields = pickleableFields
            }

        // update generated slices
        let generatedSlices = assemblyState.GeneratedSlices.Add(sliceId, sliceInfo)
        
        // update the type index
        let mapTypeIndex (id : string) (info : TypeParseInfo) =
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



    /// compiles a collection of assemblies

    let compileDynamicAssemblySlices (state : DynamicAssemblyCompilerState) (assemblies : Assembly list) =
        try
            // resolve dynamic assembly dependency graph
            let parsedDynamicAssemblies = parseDynamicAssemblies state assemblies

            // exceptions are handled explicitly so that returned state reflects the last successful compilation
            let compileSlice (state : DynamicAssemblyCompilerState, accumulator : Exn<DynamicAssemblySlice list>)
                                (typeData, dynAsmb, _, assemblyDef) =

                match accumulator with
                | Success slices ->
                    try
                        let slice, state = compileDynamicAssemblySlice state dynAsmb typeData assemblyDef
                        state, Success (slice :: slices)
                    with e ->
                        state, Error e
                | Error _ -> state, accumulator

            List.fold compileSlice (state, Success []) parsedDynamicAssemblies

        with e -> state, Error e


    /// initializes a stateful compilation actor

    let mkCompilationAgent (profiles : IDynamicAssemblyProfile list) (outpath : string) =
        let init = initCompilerState profiles outpath 
        mkStatefulActor init compileDynamicAssemblySlices