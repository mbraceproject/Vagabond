namespace Nessos.Vagrant
    
    open System
    open System.IO
    open System.Reflection

    open FsPickler

    open Nessos.Vagrant.DependencyAnalysis
    open Nessos.Vagrant.DependencyExporter

    type Utilities =
        
        static member ComputeObjectDependencies(obj:obj) =
            gatherObjectDependencies obj
            |> Seq.map fst
            |> traverseDependencies

        static member ComputeAssemblyDependencies(assembly:Assembly) = traverseDependencies [assembly]
        static member ComputeAssemblyDependencies(assemblies:seq<Assembly>) = traverseDependencies assemblies


    type VagrantClient internal (pickler : FsPickler, localServerId : Guid option) =
        static let singletonLock = ref false
        do
            lock singletonLock (fun () ->
                if !singletonLock then
                    invalidOp "Vagrant: only one instance of 'VagrantClient' permitted."
                else
                    singletonLock := true)

        let agent = mkDependencyLoader pickler localServerId

        new (?pickler : FsPickler) =
            let pickler = match pickler with None -> new FsPickler() | Some p -> p
            new VagrantClient(pickler, None)

        member __.Pickler = pickler

        member __.LoadDependencyInfo(info : DependencyInfo) = agent.Invoke info
        member __.LoadDependencyInfo(info : seq<DependencyInfo>) =
            for i in info do agent.Invoke i