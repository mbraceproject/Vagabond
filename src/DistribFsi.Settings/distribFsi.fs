namespace Nessos.DistribFsi

    open System.Reflection
    open System.IO
    open System.Text
    open System

    open Nessos.DistribFsi.DependencyAnalysis

    type private DistribFsiState =
        | Uninitialized
        | InteractionCompiler of InteractionCompilerInfo
        | ClientSerializer of ISerializer

    type DistribFsi internal () =

        let mutable state = Uninitialized

        let installState newState =
            lock state (fun () ->
                match state with
                | Uninitialized -> state <- newState
                | _ -> invalidOp "DistribFsi: registry already populated.")

        let getCompilerInfo() =
            match state with
            | InteractionCompiler info -> info
            | _ -> invalidOp "DistribFsi: No interaction compiler installed in this instance."

        let tryGetCompilerInfo() =
            match state with
            | InteractionCompiler info -> Some info
            | _ -> None

        member internal __.InstallInteractionCompiler info = installState <| InteractionCompiler info
        member __.InstallClientSerializer (serializer : ISerializer) =
            installState <| ClientSerializer serializer

        member __.IsDistributedFsiSession = 
            match state with InteractionCompiler _ -> true | _ -> false

        member __.RequestCompilation(?compilePendingInteraction) = 
            let compilePendingInteraction = defaultArg compilePendingInteraction false
            getCompilerInfo().RequestCompilation(compilePendingInteraction)

        member __.GetObjectDependencies(obj) = DependencyAnalysis.computeObjectDependencies (tryGetCompilerInfo()) obj
        member __.TryGetFsiDeclarationInfo(m : MemberInfo) = 
            match tryGetCompilerInfo() with
            | None -> None
            | Some info -> info.TryGetDeclarationInfo m

        member __.TryGetFsiDeclarationInfo(expr : Quotations.Expr) = 
            match expr with
            | SymbolExpr m -> __.TryGetFsiDeclarationInfo m
            | _ -> invalidArg "expr" "not a declaration symbol."
        
        member __.Serializer =
            match state with 
            | Uninitialized -> invalidOp "DistribFsi: No pickler has been registered in this instance."
            | InteractionCompiler info -> info.Serializer
            | ClientSerializer s -> s


    module Settings =

        let distribFsi = new DistribFsi ()

        [<assembly: AutoOpen("Nessos.DistribFsi.Settings")>]
        do()


    // referenced by the interaction compiler when pickling top-level value bindings
    type SerializationSupport private () =
        static let encoding = System.Text.Encoding.UTF8
        static member Pickle (obj:obj) = 
            use m = new MemoryStream()
            Settings.distribFsi.Serializer.Serialize m obj
            m.ToArray()

        static member UnPickle<'T> (pickle:byte[]) =
            use m = new MemoryStream(pickle)
            Settings.distribFsi.Serializer.Deserialize<obj> m :?> 'T

        static member PickleToString(obj:obj) =
            let bytes = SerializationSupport.Pickle obj
            encoding.GetString(bytes)

        static member UnPickleOfString<'T> (pickle:string) =
            let bytes = encoding.GetBytes(pickle)
            SerializationSupport.UnPickle<'T> bytes