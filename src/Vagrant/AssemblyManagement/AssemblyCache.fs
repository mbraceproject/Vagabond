module internal Nessos.Vagrant.AssemblyCache

    open System
    open System.IO
    open System.Reflection

    open Nessos.FsPickler

    open Nessos.Vagrant
    open Nessos.Vagrant.Utils

    type CachedAssemblyInfo =
        {
            Id : AssemblyId
            Location : string
            Symbols : string option
            StaticInitializer : (string * StaticInitializationInfo) option
        }

    type AssemblyCache (cacheDirectory : string, pickler : FsPicklerSerializer) =
        do
            if not <| Directory.Exists cacheDirectory then
                raise <| new DirectoryNotFoundException(cacheDirectory)

        static let metadataExt = ".vagrant"
        static let staticInitExt = ".init"
        static let symbolsExt = if runsOnMono.Value then ".mdb" else ".pdb"

        // gets a unique file name in cache directory that corresponds to assembly id.
        let getCachedAssemblyPath (id : AssemblyId) =
            let hash = Convert.toBase32String id.ImageHash
            let name = sprintf "%s-%s" (id.GetName().Name) hash
            Path.Combine(cacheDirectory, name + ".dll")

        // write dynamic assembly metadata to cache
        let writeMetadata path info =
            use fs = new FileStream(Path.ChangeExtension(path, metadataExt), FileMode.Create)
            pickler.Serialize<StaticInitializationInfo>(fs, info)
        
        // writes static initializatin data based on current state
        let writeStaticInitializer path (previous : (string * StaticInitializationInfo) option) (init : StaticInitializer) =
            match previous with
            | Some (_,p) when p.Generation > init.Generation -> previous
            | _ ->
                let info = { Generation = init.Generation ; Errors = [||] ; IsPartial = init.IsPartial }
                writeMetadata path info
                let initFile = Path.ChangeExtension(path, staticInitExt)
                File.WriteAllBytes(initFile, init.Data)
                Some(initFile, info)

        // read dynamic assembly metadata from cache
        let tryReadMetadata path =
            let mf = Path.ChangeExtension(path, metadataExt)
            if File.Exists mf then
                use fs = new FileStream(mf, FileMode.Open)
                let metadata = pickler.Deserialize<StaticInitializationInfo>(fs)
                let staticInitializer = Path.ChangeExtension(path, staticInitExt)
                if File.Exists staticInitializer then
                    Some (staticInitializer, metadata)
                else
                    None
            else
                None

        // resolve symbols file
        let tryFindSymbols path =
            let s1 = Path.ChangeExtension(path, ".pdb")
            if File.Exists s1 then Some s1
            else
                let s2 = Path.ChangeExtension(path, ".mdb")
                if File.Exists s2 then Some s2
                else None

        let getPersistedAssemblyInfo (path : string) (id : AssemblyId) =
            let symbols = tryFindSymbols path
            let metadata = tryReadMetadata path

            {
                Id = id
                Location = path
                Symbols = symbols
                StaticInitializer = metadata
            }

        member __.CacheDirectory = cacheDirectory

        member __.TryGetCachedAssemblyInfo(id : AssemblyId) =
            let cachePath = getCachedAssemblyPath id

            if File.Exists cachePath then
                Some <| getPersistedAssemblyInfo cachePath id
            else
                None

        member __.IsCachedAssembly(id : AssemblyId) =
            __.TryGetCachedAssemblyInfo(id).IsSome

        member __.GetStaticAssemblyInfo(assembly : Assembly) =
            if assembly.IsDynamic || String.IsNullOrEmpty assembly.Location then
                invalidArg assembly.FullName "assembly is dynamic or not persistable."
            else
                getPersistedAssemblyInfo assembly.Location assembly.AssemblyId

        member __.CreatePortableAssembly(cai : CachedAssemblyInfo, includeImage : bool) =

            let image =
                if includeImage then Some <| File.ReadAllBytes cai.Location
                else
                    None

            let symbols = cai.Symbols |> Option.map File.ReadAllBytes

            let staticInit =
                match cai.StaticInitializer with
                | None -> None
                | Some(path, info) ->
                    let data = File.ReadAllBytes path
                    Some {
                        Generation = info.Generation
                        Data = data
                        IsPartial = info.IsPartial
                    }

            {
                Id = cai.Id
                Image = image
                Symbols = symbols
                StaticInitializer = staticInit
            }

        member __.CreatePortableAssembly(assembly : Assembly, includeImage) =
            let info = __.GetStaticAssemblyInfo assembly
            __.CreatePortableAssembly(info, includeImage)

        member __.Cache(pa : PortableAssembly) =
            let cachePath = getCachedAssemblyPath pa.Id
            if File.Exists cachePath then
                let info = getPersistedAssemblyInfo cachePath pa.Id
                let staticInit =
                    match pa.StaticInitializer with
                    | None -> info.StaticInitializer
                    | Some init -> writeStaticInitializer cachePath info.StaticInitializer init

                { info with StaticInitializer = staticInit }

            else
                match pa.Image with
                | None -> 
                    let msg = sprintf "Portable assembly '%O' lacking image specification." pa.FullName
                    raise <| new VagrantException(msg)

                | Some img -> 
                    File.WriteAllBytes(cachePath, img)

                    let symbols =
                        match pa.Symbols with
                        | None -> None
                        | Some sym -> 
                            let symFile = Path.ChangeExtension(cachePath, symbolsExt)
                            File.WriteAllBytes(symFile, sym)
                            Some symFile

                    let staticInit =
                        match pa.StaticInitializer with
                        | None -> None
                        | Some init -> writeStaticInitializer cachePath None init

                    { Id = pa.Id ; Location = cachePath ; Symbols = symbols ; StaticInitializer = staticInit }


        member __.Cache(assembly : Assembly, ?asId : AssemblyId) =
            let info = __.GetStaticAssemblyInfo assembly
            let id = defaultArg asId info.Id
            let cachePath = getCachedAssemblyPath id
            if File.Exists cachePath then
                getPersistedAssemblyInfo cachePath id
            else
                File.Copy(assembly.Location, cachePath)
                let symbols =
                    match info.Symbols with
                    | None -> None
                    | Some s ->
                        let symFile = Path.ChangeExtension(cachePath, symbolsExt)
                        File.Copy(s, symFile)
                        Some symFile

                { Id = id ; Location = cachePath ; Symbols = symbols ; StaticInitializer = None }