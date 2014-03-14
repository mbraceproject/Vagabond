module internal Nessos.Vagrant.BlobManagement

    open System
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils

    type private FieldData = (FieldInfo * Exn<byte []>)

    let mkDependencyInfo (pickler : FsPickler)
                            (state : DynamicAssemblyCompilerState) 
                            (generationIdx : Map<string, int>)
                            (generateTypeInitializationBlob : bool)
                            (assembly : Assembly) =

        match state.TryFindSliceInfo assembly.FullName with
        | None -> invalidArg assembly.FullName "supplied assembly is not a dynamic assembly slice."
        | Some (staticFields, sliceInfo) when generateTypeInitializationBlob ->

            // create pickled type initializers
            let tryPickle (fI : FieldInfo) =
                try
                    let value = fI.GetValue(null)
                    let pickle = pickler.Pickle<obj>(value)
                    fI, Success pickle
                with e -> 
                    fI, Error e

            let gen = 1 + defaultArg (generationIdx.TryFind assembly.FullName) 0
            let data = Array.map tryPickle staticFields
            let errors = data |> Array.choose (function fI, Error e -> Some(fI, e) | _ -> None)
            let blob = pickler.Pickle<FieldData []> data
            let sliceInfo = { sliceInfo with TypeInitializationBlob = Some(gen, blob) }
            let generationIdx = generationIdx.Add(assembly.FullName, gen)

            generationIdx, (errors, sliceInfo)

        | Some (_,sliceInfo) -> generationIdx, ([||], sliceInfo)


    let loadDependencyInfo (pickler : FsPickler) (localServerId : Guid option) (loadState : Map<string, int>) (info : DynamicAssemblySliceInfo) =
        match localServerId, info.TypeInitializationBlob with
        | Some id, _ when id = info.SourceId -> loadState, [||]
        | _, None -> loadState, [||]
        | _, Some(gen,blob) ->
            match loadState.TryFind info.Assembly.FullName with
            | Some loadedGen when loadedGen >= gen -> loadState, [||]
            | _ ->
                let tryLoad ((fI, data) : FieldData) =
                    match data with
                    | Success bytes -> 
                        try 
                            let value = pickler.UnPickle<obj> bytes
                            fI.SetValue(null, value) ; None
                        with e -> Some (fI, e)
                    | Error e -> Some (fI, e)

                let data = pickler.UnPickle<FieldData []>(blob)
                let errors = Array.choose tryLoad data
                let loadState = loadState.Add(info.Assembly.FullName, gen)

                loadState, errors


    let mkExporterAgent (pickler : FsPickler) (stateF : unit -> DynamicAssemblyCompilerState) =
        mkStatefulAgent Map.empty (fun state (mkBlob, a) -> mkDependencyInfo pickler (stateF()) state mkBlob a)

    let mkLoaderAgent (pickler : FsPickler) (localServerId : Guid option) =
        mkStatefulAgent Map.empty (fun state i -> loadDependencyInfo pickler localServerId state i)