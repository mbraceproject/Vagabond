namespace Nessos.Vagrant
    
    open System
    open System.Text.RegularExpressions
    open System.Reflection

    open Microsoft.FSharp.Control

    open FsPickler

    open Nessos.Vagrant.Serialization
    open Nessos.Vagrant.FsiAssemblyCompiler
    open Nessos.Vagrant.DependencyAnalysis

    type internal ServerMsg = obj * AsyncReplyChannel<Choice<PortableDependencyInfo * (exn * FieldInfo) list, exn>>
    type internal ClientMsg = PortableDependencyInfo * AsyncReplyChannel<exn option>





    type CompilationClient private (?pickler : FsPickler) =
        
        let pickler = match pickler with None -> new FsPickler() | Some p -> p

        let rec actorBehaviour (mailbox : MailboxProcessor<ClientMsg>) = async {
            let! info, rc = mailbox.Receive()

            let reply =
                try loadPortableDependencyInfo pickler info ; None
                with e -> Some e

            rc.Reply reply

            return! actorBehaviour mailbox
        }

        let actor = MailboxProcessor.Start actorBehaviour

        let loadDependencyInfo (info : PortableDependencyInfo) =
            match actor.PostAndReply <| fun ch -> info,ch with
            | None -> ()
            | Some e -> raise e

        member __.Pickler = pickler
        member __.LoadPortableDependencyInfo (info : PortableDependencyInfo) =
            loadDependencyInfo info

        member __.LoadPortablePickle (pickle : Pickle) =
            loadDependencyInfo pickle.DependencyInfo
            pickler.UnPickle<obj> pickle.Pickle

        static member Create(?pickler : FsPickler) = new CompilationClient(?pickler = pickler)



    type CompilationServer private (?picklerRegistry : CustomPicklerRegistry, ?path : string) as self =
        
        static let singletonLock = ref None
        do lock singletonLock
            (fun () ->
                if singletonLock.Value.IsSome then invalidOp "A compilation server instance is already running."
                else singletonLock := Some self)

        let serverId = Guid.NewGuid().ToString()
        let path = defaultArg path <| System.IO.Path.GetTempPath()
        // TODO : check path
        let globalStateContainer = ref <| GlobalDynamicAssemblyState.Init(serverId, path)

        let assemblyRegex = Regex(sprintf "^(.*)_%s_[0-9]*$" serverId)

        let typeNameConverter =
            {
                new DistribFsiNameConverter() with
                    member __.DynamicAssemblyState = !globalStateContainer
                    member __.TryGetDynamicAssemblyNameOfSlice sliceName =
                        let m = assemblyRegex.Match sliceName
                        if m.Success then Some <| m.Groups.[1].Value
                        else
                            None
            }

        let fspickler = mkFsPicklerInstance picklerRegistry typeNameConverter

        let client = CompilationClient.Create(fspickler)

        // use mailbox processor to sequentialize compilation requests
        let rec compiler (mailbox : MailboxProcessor<ServerMsg>) = async {
            let! obj, rc = mailbox.Receive()

            let result =
                try
                    let assemblies, dynamicAssemblies, state = computeObjectDependencies globalStateContainer.Value obj
                    // update the state container *before* producing the blobs;
                    // this is required because the serializer uses it
                    do globalStateContainer := state
                    // it is now safe to perform serializations
                    let info, errors = getPortableDependencyInfo fspickler serverId assemblies dynamicAssemblies

                    Choice1Of2 (info, errors)
                with e -> Choice2Of2 e

            rc.Reply result

            return! compiler mailbox
        }

        let compilationActor = MailboxProcessor.Start compiler

        let computeDependencies (obj:obj) =
            match compilationActor.PostAndReply <| fun ch -> obj,ch with
            | Choice1Of2 assemblies -> assemblies
            | Choice2Of2 e -> raise e

        member __.ComputePortableDependencies (obj:obj) = computeDependencies obj
        member __.ComputePortablePickle (obj:obj) =
            let info, errors = computeDependencies obj
            let pickle = {
                Pickle = fspickler.Pickle<obj> obj
                DependencyInfo = info
            }

            pickle, errors


        member __.Pickler = fspickler
        member __.Client = client

        static member Create (?picklerRegistry : CustomPicklerRegistry, ?path : string) =
            new CompilationServer(?picklerRegistry = picklerRegistry, ?path = path)


            
        