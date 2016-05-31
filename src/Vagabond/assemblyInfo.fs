namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.13.0")>]
[<assembly: AssemblyFileVersionAttribute("0.13.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.13.0"
    let [<Literal>] InformationalVersion = "0.13.0"
