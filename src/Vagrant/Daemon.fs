module internal Nessos.Vagrant.Daemon

    open System
    open System.Reflection

    open Microsoft.FSharp.Control

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.SliceCompilerTypes
    open Nessos.Vagrant.SliceCompiler
    open Nessos.Vagrant.AssemblyExporter
    open Nessos.Vagrant.AssemblyCache
    open Nessos.Vagrant.AssemblyLoader

    type VagrantState =
        {
            CompilerState : DynamicAssemblyCompilerState
            AssemblyExporterState : ExporterState
            AssemblyLoaderState : AssemblyLoadState

            Pickler : BasePickler
            CacheDirectory : string
        }

    type VagrantMessage = 
        | LoadAssembly of AssemblyLoadPolicy * PortableAssembly * ReplyChannel<AssemblyLoadInfo>
        | GetPortableAssembly of AssemblyLoadPolicy * AssemblyId * ReplyChannel<PortableAssembly>
        | GetAssemblyLoadInfo of AssemblyLoadPolicy * AssemblyId * ReplyChannel<AssemblyLoadInfo>
        | CompileDynamicAssemblySlice of Assembly list * ReplyChannel<DynamicAssemblySlice list>


    let rec vagrantDaemonBehaviour (state : VagrantState) (inbox : MailboxProcessor<VagrantMessage>) = async {

        let! message = inbox.Receive()

        match message with
        | CompileDynamicAssemblySlice (assemblies, rc) ->
            let compilerState, result = compileDynamicAssemblySlices state.CompilerState assemblies
            rc.Reply result
            return! vagrantDaemonBehaviour { state with CompilerState = compilerState } inbox

        | GetPortableAssembly (policy, assemblyId, rc) ->
            match state.CompilerState.TryFindSliceInfo
    
    
    }