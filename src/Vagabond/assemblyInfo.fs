namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.12.1")>]
[<assembly: AssemblyFileVersionAttribute("0.12.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.12.1"
