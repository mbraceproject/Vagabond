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

        let compilationAgent =
            let init = initGlobalState outpath
            mkStatefulWrapper init compileDynamicAssemblySlices

        let serverId = compilationAgent.CurrentState.ServerId

        let getState () = compilationAgent.CurrentState

        let pickler = mkFsPicklerInstance picklerRegistry getState

        let exportingAgent = mkDependencyExporter pickler getState
        let client = new VagrantClient(pickler, Some serverId)

        let compile (assemblies : Assembly list) =
            match compilationAgent.Invoke assemblies with
            | Choice1Of2 slices -> slices
            | Choice2Of2 e -> raise e

        member __.UUId = serverId
        member __.Pickler = pickler
        member __.Client = client

        member __.CompileDynamicAssemblySlice (assembly : Assembly) =
            if assembly.IsDynamic then
                let newSlices = compile [assembly]
                newSlices |> List.map (fun slice -> exportingAgent.Invoke slice.Assembly)

            else invalidArg assembly.FullName "Vagrant: not a dynamic assembly."

        member __.ResolveDynamicDependenciesRequiringCompilation(obj : obj) : Assembly list =
            let dependencies = gatherObjectDependencies obj
            getDynamicDependenciesRequiringCompilation compilationAgent.CurrentState dependencies

        member __.GetDynamicAssemblySlices(assembly : Assembly) =
            match compilationAgent.CurrentState.DynamicAssemblies.TryFind assembly.FullName with
            | None -> []
            | Some info ->
                info.GeneratedSlices 
                |> List.sortBy (fun s -> s.SliceId) 
                |> List.map (fun s -> s.Assembly)

        member __.GetDependencyInfo(assembly : Assembly) = exportingAgent.Invoke assembly

        member __.ComputeObjectDependencies(obj : obj, ?allowCompilation : bool) : DependencyInfo list =
            let allowCompilation = defaultArg allowCompilation false

            let dependencies = gatherObjectDependencies obj

            if allowCompilation then
                let assemblies = getDynamicDependenciesRequiringCompilation compilationAgent.CurrentState dependencies
                let _ = compile assemblies in ()

            let remapped = remapDependencies compilationAgent.CurrentState dependencies

            remapped |> List.map exportingAgent.Invoke