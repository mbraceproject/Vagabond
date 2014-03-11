module Nessos.Vagrant.DependencyExporter

    open System
    open System.Reflection

    open Microsoft.FSharp.Control

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils
    open Nessos.Vagrant.SliceCompiler

    open FsPickler

    let mkDependencyInfo (pickler : FsPickler) 
                            (state : GlobalDynamicAssemblyState) 
                            (generationIdx : Map<string, int>) 
                            (assembly : Assembly) =

        assert (not assembly.IsDynamic)

        match state.TryGetDynamicAssemblyName assembly.FullName with
        | None ->
            let info =
                {
                    Assembly = assembly
            
                    SourceId = Guid.Empty
                    SliceId = 0
                    IsDynamicAssemblySlice = false
                    BlobGeneration = 0
                    TypeInitializationBlobs = []
                    TypeInitializationErrors = []
                    ActualQualifiedName = assembly.FullName
                }

            info, generationIdx

        | Some dynamicAssembly ->
            let sliceInfo = 
                state.DynamicAssemblies.[dynamicAssembly].GeneratedSlices 
                |> List.find (fun s -> s.Assembly = assembly)

            let fieldPickles, pickleFailures =
                sliceInfo.StaticFields
                |> List.map(fun (sourceField, targetField) -> 
                    try
                        let value = sourceField.GetValue(null)
                        let pickle = pickler.Pickle<obj>(value)
                        Choice1Of2 (targetField, pickle)
                    with e -> Choice2Of2 (targetField, e.ToString()))
                |> Choice2.partition

            let generation = 1 + defaultArg (generationIdx.TryFind assembly.FullName) 0

            let info =
                {
                    SourceId = state.ServerId
                    SliceId = sliceInfo.SliceId
                    ActualQualifiedName = sliceInfo.DynamicAssemblyName

                    IsDynamicAssemblySlice = true
                    Assembly = sliceInfo.Assembly
                    BlobGeneration = generation
                    TypeInitializationBlobs = fieldPickles
                    TypeInitializationErrors = pickleFailures
                }

            let generationIdx = generationIdx.Add(assembly.FullName, generation)

            info, generationIdx


    let loadDependencyInfo (pickler : FsPickler) (info : DependencyInfo) =
        for fI, blob in info.TypeInitializationBlobs do
            let value = pickler.UnPickle<obj>(blob)
            fI.SetValue(null, value)


    type ExporterMsg = Assembly * AsyncReplyChannel<Choice<DependencyInfo, exn>>

    type DependencyExporter (compiler : SliceCompilationServer, pickler : FsPickler) =

        let blobGenerationCounter = ref Map.empty<string, int>

        let rec export (mailbox : MailboxProcessor<ExporterMsg>) = async {
            let! assembly,rc = mailbox.Receive()

            let reply =
                try
                    let info, counter = mkDependencyInfo pickler compiler.State blobGenerationCounter.Value assembly

                    do blobGenerationCounter := counter

                    Choice1Of2 info

                with e -> Choice2Of2 e

            rc.Reply reply

            return! export mailbox
        }

        let exporter = MailboxProcessor.Start(export)

        member __.GetAssemblyInfo (a : Assembly) =
            match exporter.PostAndReply <| fun ch -> a,ch with
            | Choice1Of2 info -> info
            | Choice2Of2 e -> raise e



    type LoaderMsg = DependencyInfo * AsyncReplyChannel<exn option>

    type DependencyLoader (serverId : Guid option, pickler : FsPickler) =
        
        let genCounter = ref Map.empty<string, int>

        let rec load (mailbox : MailboxProcessor<LoaderMsg>) = async {
        
            let! info, rc = mailbox.Receive()

            match serverId, genCounter.Value.TryFind info.Assembly.FullName with
            | Some id, _ when id = info.SourceId -> rc.Reply None
            | _, Some gen when gen >= info.BlobGeneration -> rc.Reply None
            | _ ->
                let reply =
                    try loadDependencyInfo pickler info ; None
                    with e -> Some e

                genCounter := genCounter.Value.Add(info.Assembly.FullName, info.BlobGeneration)

                rc.Reply reply

            return! load mailbox
        }


        let loader = MailboxProcessor.Start load

        member __.LoadDependencyInfo (info : DependencyInfo) =
            match loader.PostAndReply <| fun ch -> info, ch with
            | None -> ()
            | Some e -> raise e