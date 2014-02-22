#r "bin/Debug/DistribFsi.Settings.dll"
#r "../../samples/ThunkServer/bin/Debug/ThunkServer.exe"

open System
open System.Reflection

open Nessos.DistribFsi.Sample

let ass = Assembly.GetExecutingAssembly()

let lsType (t : Type) =
    let modl = t.FullName.Split([|'.';'+'|]).[0]
    let types = ass.GetTypes() |> Array.filter (fun t -> t.Name.Contains modl)
    types |> Array.collect (fun t -> t.GetFields(BindingFlags.Static ||| BindingFlags.NonPublic))
    
    
type Marker = class end

let f =
    let r = System.Random()
    let x = r.Next()
    let y = let x = r.Next() in x + 1
    fun () -> x + y

let f0 = Func f

f ()


let client = initThunkServer()

client.EvaluateThunk f0





let _ = distribFsi.RequestCompilation()


