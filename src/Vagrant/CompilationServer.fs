namespace Nessos.Vagrant
    
    open System
    open System.IO
    open System.Text.RegularExpressions
    open System.Reflection

    open Microsoft.FSharp.Control

    open FsPickler

    open Nessos.Vagrant.Serialization
    open Nessos.Vagrant.SliceCompiler

//    type internal ServerMsg = obj * AsyncReplyChannel<Choice<PortableDependencyInfo * (exn * FieldInfo) list, exn>>
//    type internal ClientMsg = PortableDependencyInfo * AsyncReplyChannel<exn option>
//
//
        
    type internal ServerMsg = CompileNextSlice of Assembly * AsyncReplyChannel<Choice<AssemblySliceInfo option, exn>>


//    type CompilationClient private (?pickler : FsPickler) =
//        
//        let pickler = match pickler with None -> new FsPickler() | Some p -> p
//
//        let rec actorBehaviour (mailbox : MailboxProcessor<ClientMsg>) = async {
//            let! info, rc = mailbox.Receive()
//
//            let reply =
//                try loadPortableDependencyInfo pickler info ; None
//                with e -> Some e
//
//            rc.Reply reply
//
//            return! actorBehaviour mailbox
//        }
//
//        let actor = MailboxProcessor.Start actorBehaviour
//
//        let loadDependencyInfo (info : PortableDependencyInfo) =
//            match actor.PostAndReply <| fun ch -> info,ch with
//            | None -> ()
//            | Some e -> raise e
//
//        member __.Pickler = pickler
//        member __.LoadPortableDependencyInfo (info : PortableDependencyInfo) =
//            loadDependencyInfo info
//
//        member __.LoadPortablePickle (pickle : Pickle) =
//            loadDependencyInfo pickle.DependencyInfo
//            pickler.UnPickle<obj> pickle.Pickle
//
//        static member Create(?pickler : FsPickler) = new CompilationClient(?pickler = pickler)



    type CompilationServer private (?picklerRegistry : CustomPicklerRegistry, ?outPath : string) as self =

        let outPath = 
            match outPath with
            | Some path when Directory.Exists path -> path
            | Some _ -> invalidArg "outPath" "not a directory."
            | None -> Path.GetTempPath()

        static let singletonLock = ref None
        do lock singletonLock
            (fun () ->
                if singletonLock.Value.IsSome then invalidOp "A compilation server instance is already running."
                else singletonLock := Some self)

        let globalStateContainer = ref <| GlobalDynamicAssemblyState.Init(outPath)

        let assemblyRegex = Regex(sprintf "^(.*)_%O_[0-9]*$" globalStateContainer.Value.ServerId)

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

        let pickler = mkFsPicklerInstance picklerRegistry typeNameConverter

        // use mailbox processor to sequentialize compilation requests
        let rec compiler (mailbox : MailboxProcessor<ServerMsg>) = async {
            let! (CompileNextSlice(assembly, rc)) = mailbox.Receive()

            let result =
                try
                    let sliceInfo, state = tryCompileDynamicAssemblySlice globalStateContainer.Value assembly
                    do globalStateContainer := state
                    Choice1Of2 sliceInfo
                with e -> Choice2Of2 e

            rc.Reply result

            return! compiler mailbox
        }

        let compilationActor = MailboxProcessor.Start compiler

        let tryCompileNextSlice (a : Assembly) =
            match compilationActor.PostAndReply <| fun ch -> CompileNextSlice(a,ch) with
            | Choice1Of2 slice -> slice
            | Choice2Of2 e -> raise e

        member __.TryCompileNextSlice (a : Assembly) = tryCompileNextSlice(a : Assembly)

//        member __.ComputePortableDependencies (obj:obj) = computeDependencies obj
//        member __.ComputePortablePickle (obj:obj) =
//            let info, errors = computeDependencies obj
//            let pickle = {
//                Pickle = fspickler.Pickle<obj> obj
//                DependencyInfo = info
//            }
//
//            pickle, errors


        member __.Pickler = pickler

//        static member Create (?picklerRegistry : CustomPicklerRegistry, ?path : string) =
//            new CompilationServer(?picklerRegistry = picklerRegistry, ?path = path)
//
//
//            
//        