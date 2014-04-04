module internal Nessos.Vagrant.StaticInitialization


    

//    open System
//    open System.Reflection
//
//    open Nessos.FsPickler
//
//    open Nessos.Vagrant
//    open Nessos.Vagrant.Utils
//
//    let getStaticInitializationData (pickler : FsPickler) (state : DynamicAssemblyCompilerState) 
//                                        (generationIdx : Map<string, int>) (assembly : Assembly) =
//
//        match state.TryFindSliceInfo assembly.FullName with
//        | None -> invalidArg assembly.FullName "supplied assembly is not a dynamic assembly slice."
//        | Some (staticFields, sliceInfo) ->
//
//            // create pickled type initializers
//            let tryPickle (fI : FieldInfo) =
//                try
//                    let value = fI.GetValue(null)
//                    let pickle = pickler.Pickle<obj>(value)
//                    Choice1Of2 (fI, pickle)
//                with e -> 
//                    Choice2Of2 (fI, e)
//
//            let pickles, errors = staticFields |> Array.map tryPickle |> Seq.toList |> Choice.partition2
//            let generation = 1 + defaultArg (generationIdx.TryFind assembly.FullName) -1
//            let generationIdx = generationIdx.Add(assembly.FullName, generation)
//
//            let data =
//                {
//                    Generation = generation
//                    Data = List.toArray pickles
//                    Errors = List.toArray errors
//                }
//
//            generationIdx, { sliceInfo with StaticInitializationData = Some data }
//
//
//    let loadStaticInitializationData (pickler : FsPickler) (localServerId : Guid option) (loadState : Map<string, int>) (info : DynamicAssemblySlice) =
//        match localServerId, info.StaticInitializationData with
//        | Some id, _ when id = info.SourceId -> loadState, [||]
//        | _, None -> loadState, [||]
//        | _, Some initializer ->
//            match loadState.TryFind info.Assembly.FullName with
//            | Some loadedGen when loadedGen >= initializer.Generation -> loadState, [||]
//            | _ ->
//                let tryLoad (fI : FieldInfo, bytes : byte []) =
//                    try 
//                        let value = pickler.UnPickle<obj> bytes
//                        fI.SetValue(null, value) ; None
//                    with e -> Some (fI, e)
//
//                let errors = Array.choose tryLoad initializer.Data
//
//                let loadState = loadState.Add(info.Assembly.FullName, initializer.Generation)
//
//                loadState, errors
//
//
//    let mkExporterAgent (pickler : FsPickler) (stateF : unit -> DynamicAssemblyCompilerState) =
//        mkStatefulAgent Map.empty (fun state a -> getStaticInitializationData pickler (stateF()) state a)
//
//    let mkLoaderAgent (pickler : FsPickler) (localServerId : Guid option) =
//        mkStatefulAgent Map.empty (fun state i -> loadStaticInitializationData pickler localServerId state i)