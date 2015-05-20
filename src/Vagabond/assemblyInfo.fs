namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.6.7")>]
[<assembly: AssemblyFileVersionAttribute("0.6.7")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.6.7"
