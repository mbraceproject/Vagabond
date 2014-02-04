namespace Nessos.DistribFsi

    open FsPickler
    open Nessos.DistribFsi

    type private RegistryState =
        | Empty
        | InteractionCompiler of InteractionCompilerInfo
        | ClientPickler of FsPickler

    type DistribFsiRegistry private () =

        static let mutable state = Empty

        static let installNewState newState =
            lock state (fun () ->
                match state with
                | Empty -> state <- newState
                | _ -> invalidOp "DistribFsi: registry already populated.")

        static member InstallInteractionCompiler info = installNewState <| InteractionCompiler info
        static member InstallClientSidePickler (?pickler : FsPickler) =
            let pickler = match pickler with None -> new FsPickler() | Some p -> p
            installNewState <| ClientPickler pickler


        static member TryGetInteractionCompilerInfo () =
            match state with
            | InteractionCompiler info -> Some info
            | _ -> None

        static member InteractionCompilerInfo =
            match state with
            | InteractionCompiler info -> info
            | _ -> invalidOp "DistribFsi: No interaction compiler installed in this instance."

        static member InstalledPickler = 
            match state with 
            | Empty -> invalidOp "DistribFsi: No pickler has been registered in this instance."
            | InteractionCompiler info -> info.Pickler
            | ClientPickler p -> p


    // referenced by the interaction compiler when pickling top-level values
    type DeserializationSupport =
        static member Deserialize (pickle : byte []) = DistribFsiRegistry.InstalledPickler.UnPickle<obj> pickle :?> 'T