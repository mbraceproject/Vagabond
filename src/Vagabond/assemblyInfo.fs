namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.13.1")>]
[<assembly: AssemblyFileVersionAttribute("0.13.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.13.1"
    let [<Literal>] InformationalVersion = "0.13.1"
