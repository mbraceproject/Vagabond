(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "FsPickler.dll"
#r "Vagabond.Cecil.dll"
#r "Vagabond.dll"

open System
open System.Reflection

let assembly = Unchecked.defaultof<Assembly>

(**
# API Overview

The included implementation of
[ThunkServer](https://github.com/nessos/Vagabond/blob/master/samples/ThunkServer/thunkServer.fsx) 
is a straightforward distributed implementation that makes use of Vagabond.
What follows is a brief overview of the basic API.

A Vagabond environment can be initialized as follows:

*)

open Nessos.Vagabond

let vagabond = Vagabond.Initialize(cacheDirectory = "/tmp/vagabond")

(** Given an arbitrary object, dependencies are resolved like so: *)

let value = [ Some(fun x -> printfn "%d" x; x + 1) ; None ; Some id ]

let assemblies = vagabond.ComputeObjectDependencies(value, permitCompilation = true)

(** An assembly can be exported by writing *)

let assemblyPackage = vagabond.CreateAssemblyPackage(assembly, includeAssemblyImage = true)

(** 

An assembly package contains necessary data to load the specified assembly in a remote process.

Assemblies can be loaded in a remote process like so:

*)

let response : AssemblyLoadInfo = vagabond.LoadAssemblyPackage assemblyPackage

(**

## Communication

Once all required dependencies have been loaded, communication can be 
established by using the ``.Pickler`` property found in both the server
and client instances. These give an instance of ``FsPickler`` that is
capable of serializing and deserializing objects depending on dynamic assemblies.

*)

let pickler = vagabond.Pickler

let bytes = pickler.Pickle(value)