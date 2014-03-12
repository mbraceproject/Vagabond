namespace Nessos.Vagrant

    open System
    open System.IO
    open System.Reflection

    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.Serialization
    open Nessos.Vagrant.DependencyAnalysis
    open Nessos.Vagrant.DependencyExporter
    open Nessos.Vagrant.SliceCompiler

    open FsPickler

    type VagrantServer(?outpath : string, ?picklerRegistry : CustomPicklerRegistry) =

        let outpath = 
            match outpath with
            | Some path when Directory.Exists path -> path
            | Some _ -> invalidArg "outPath" "not a valid directory."
            | None -> Path.GetTempPath()

        static let singletonLock = ref false
        do
            lock singletonLock (fun () ->
                if !singletonLock then
                    invalidOp "Vagrant: only one instance of 'VagrantServer' permitted."
                else
                    singletonLock := true)

        // initialize agents

        let compiler = mkCompilationAgent outpath
        let pickler = mkFsPicklerInstance picklerRegistry (fun () -> compiler.CurrentState)
        let exporter = mkExporterAgent pickler (fun () -> compiler.CurrentState)
        let client = new VagrantClient(pickler, Some compiler.CurrentState.ServerId)

        let compile (assemblies : Assembly list) =
            match compiler.Invoke assemblies with
            | Choice1Of2 slices -> slices
            | Choice2Of2 e -> raise e

        member __.UUId = compiler.CurrentState.ServerId
        member __.Pickler = pickler
        member __.Client = client

        member __.CompileDynamicAssemblySlice (assembly : Assembly) =
            if assembly.IsDynamic then
                let newSlices = compile [assembly]
                newSlices |> List.map (fun slice -> exporter.Invoke slice.Assembly)

            else invalidArg assembly.FullName "Vagrant: not a dynamic assembly."

        member __.ResolveDynamicDependenciesRequiringCompilation(obj : obj) : Assembly list =
            let dependencies = gatherObjectDependencies obj
            getDynamicDependenciesRequiringCompilation compiler.CurrentState dependencies

        member __.GetDynamicAssemblySlices(assembly : Assembly) =
            match compiler.CurrentState.DynamicAssemblies.TryFind assembly.FullName with
            | None -> []
            | Some info ->
                info.GeneratedSlices
                |> Map.toSeq
                |> Seq.sortBy (fun (_,s) -> s.SliceId) 
                |> Seq.map (fun (_,s) -> s.Assembly)
                |> Seq.toList

        member __.GetDependencyInfo(assembly : Assembly) = exporter.Invoke assembly

        member __.ComputeObjectDependencies(obj : obj, ?allowCompilation : bool) : DependencyInfo list =
            let allowCompilation = defaultArg allowCompilation false

            let dependencies = gatherObjectDependencies obj

            if allowCompilation then
                let assemblies = getDynamicDependenciesRequiringCompilation compiler.CurrentState dependencies
                let _ = compile assemblies in ()

            let remapped = remapDependencies compiler.CurrentState dependencies

            remapped |> List.map exporter.Invoke