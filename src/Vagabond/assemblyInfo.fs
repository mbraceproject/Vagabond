namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.5.3")>]
[<assembly: AssemblyFileVersionAttribute("0.5.3")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.5.3"
