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
//            BlobGeneration : int
            TypeInitializationBlobs : (FieldInfo * byte []) list
            TypeInitializationErrors : (FieldInfo * string) list
        }


//            DynamicAssemblySliceInfo : DynamicAssemblySliceInfo option
//        }
//
//    with
//        member __.IsDynamicAssemblySlice = __.DynamicAssemblySliceInfo.IsSome
//
//    and DynamicAssemblySliceInfo =
//        {
//            SourceId : string
//            Generation : int
//            TypeInitializationBlobs : (string * byte []) []
//        }


//    type PortableDependencyInfo =
//        {
//            SourceId : string
//            AllDependencies : Assembly list
//            DynamicAssemblies : DynamicAssemblyInfo list
//        }
//
//    and Pickle =
//        {
//            Pickle : byte []
//            DependencyInfo : PortableDependencyInfo
//        }
//
//    and DynamicAssemblyInfo =
//        {
//            ActualName : string
//            Iteration : int
//            Slices : Assembly list
//            ValueInitializationBlobs : (string * byte []) list
//        }



    // internal types

    type AssemblySliceInfo =
        {
            Assembly : Assembly
            SliceId : int
            StaticFields : (FieldInfo * FieldInfo) list
        }

    type DynamicAssemblyState =
        {
            DynamicAssembly : Assembly
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
                GeneratedSlices = []
                TypeIndex = Map.empty
            }

    and GlobalDynamicAssemblyState =
        {
            ServerId : Guid
            OutputDirectory : string
            DynamicAssemblies : Map<string, DynamicAssemblyState>
        }
    with
        static member Init(outputDirectory : string) =
            {
                ServerId = Guid.NewGuid()
                OutputDirectory = outputDirectory
                DynamicAssemblies = Map.empty
            }