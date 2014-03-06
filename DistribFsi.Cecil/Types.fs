namespace Nessos.DistribFsi

    open System
    open System.Reflection

    type AssemblyId =
        {
            Name : string
            FullName : string
            Hash : byte []
        }

    and AssemblyInfo =
        {
            Id : AssemblyId
            Location : string
        }