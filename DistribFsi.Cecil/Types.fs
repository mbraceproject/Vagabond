namespace Nessos.DistribFsi

    open System
    open System.Reflection

    type Pickle =
        {
            Pickle : byte []

            Dependencies : Assembly list
            ValueInitializationBlobs : (string * byte []) list
        }


    and internal DynamicAssemblyInfo =
        {
            Assembly : Assembly
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
                CompiledAssemblies = []
                TypeIndex = Map.empty
            }

    and internal GlobalDynamicAssemblyState =
        {
            ClientId : string
            OutputDirectory : string
            DynamicAssemblies : Map<string, DynamicAssemblyInfo>
        }