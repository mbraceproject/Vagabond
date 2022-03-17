namespace MBrace.Vagabond

open System.Reflection

#if NETSTANDARD
type AssemblyLoadContext (name : string, ?isCollectible : bool) =
    member x.Name = name
    member x.Assemblies : seq<_> = System.AppDomain.CurrentDomain.GetAssemblies()

    member x.LoadFromAssemblyName(asmName : AssemblyName) = System.AppDomain.CurrentDomain.Load(asmName)
    member x.LoadFromAssemblyPath(path : string) = Assembly.LoadFile(path)
    member x.Unload() = ()

    abstract member Load : asmName : AssemblyName -> Assembly
    default x.Load(asmName : AssemblyName) = x.LoadFromAssemblyName(asmName)

    static member GetLoadContext(asm : Assembly) = new AssemblyLoadContext(System.Guid.NewGuid() |> string)
#endif
