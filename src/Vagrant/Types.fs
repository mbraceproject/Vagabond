namespace Nessos.Vagrant

    open System
    open System.Reflection


    type DependencyInfo =
        {
            Assembly : Assembly
            SourceId : Guid
            IsDynamicAssemblySlice : bool
            ActualQualifiedName : string
            SliceId : int
            BlobGeneration : int
            TypeInitializationBlobs : (FieldInfo * byte []) list
            TypeInitializationErrors : (FieldInfo * string) list
        }


    and internal AssemblySliceInfo =
        {
            Assembly : Assembly
            DynamicAssemblyName : string
            SliceId : int
            StaticFields : (FieldInfo * FieldInfo) list
        }

    and internal DynamicAssemblyState =
        {
            DynamicAssembly : Assembly
            AssemblyReferences : Assembly list
            GeneratedSlices : AssemblySliceInfo list
            TypeIndex : Map<string, AssemblySliceInfo>
        }
    with
        member i.HasFreshTypes =
            let assemblyTypeCount = i.DynamicAssembly.GetTypes().Length
            let compiledTypeCount = i.TypeIndex.Count
            assemblyTypeCount > compiledTypeCount

        member i.Name = i.DynamicAssembly.GetName()

        static member Init(a : Assembly) =
            {
                DynamicAssembly = a
                AssemblyReferences = []
                GeneratedSlices = []
                TypeIndex = Map.empty
            }

    and internal GlobalDynamicAssemblyState =
        {
            ServerId : Guid
            OutputDirectory : string
            DynamicAssemblies : Map<string, DynamicAssemblyState>

            TryGetDynamicAssemblyName : string -> string option
            GetAssemblySliceName : string -> int -> string
        }