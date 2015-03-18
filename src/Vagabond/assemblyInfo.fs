namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.5.0")>]
[<assembly: AssemblyFileVersionAttribute("0.5.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.5.0"
