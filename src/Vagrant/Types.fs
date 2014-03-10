namespace Nessos.Vagrant

    open System
    open System.Reflection


    type PortableDependencyInfo =
        {
            SourceId : string
            AllDependencies : Assembly list
            DynamicAssemblies : ExportedDynamicAssemblyInfo list
        }

    and Pickle =
        {
            Pickle : byte []
            DependencyInfo : PortableDependencyInfo
        }

    and ExportedDynamicAssemblyInfo =
        {
            ActualName : string
            Iteration : int
            Slices : Assembly list
            ValueInitializationBlobs : (string * byte []) list
        }



    // internal types

    type DynamicAssemblyInfo =
        {
            Assembly : Assembly
            StaticFields : FieldInfo list
            CompiledAssemblies : Assembly list
            TypeIndex : Map<string, Assembly>
        }
    with
        member i.HasFreshTypes =
            let assemblyTypeCount = i.Assembly.GetTypes().Length
            let compiledTypeCount = i.TypeIndex.Count
            assemblyTypeCount > compiledTypeCount

        member i.Name = i.Assembly.GetName()

        static member Init(a : Assembly) =
            {
                Assembly = a
                StaticFields = []
                CompiledAssemblies = []
                TypeIndex = Map.empty
            }

    and GlobalDynamicAssemblyState =
        {
            ServerId : string
            OutputDirectory : string
            DynamicAssemblies : Map<string, DynamicAssemblyInfo>
        }
    with
        static member Init(serverId : string, outputDirectory : string) =
            {
                ServerId = serverId
                OutputDirectory = outputDirectory
                DynamicAssemblies = Map.empty
            }