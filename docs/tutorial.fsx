(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../tests/Vagabond.Tests/bin/Release/net6.0"
#r "FsPickler.dll"
#r "Vagabond.AssemblyParser.dll"
#r "Vagabond.dll"

open System
open System.Reflection
open MBrace.Vagabond

let assembly = Unchecked.defaultof<AssemblyId>

(**
# API Overview

The included implementation of
[ThunkServer](https://github.com/mbraceproject/Vagabond/blob/master/samples/ThunkServer/thunkServer.fsx) 
is a straightforward distributed implementation that makes use of Vagabond.
What follows is a brief overview of the basic API.

A Vagabond environment can be initialized as follows:

*)

open MBrace.Vagabond

let vmanager = Vagabond.Initialize(cacheDirectory = "/tmp/vagabond")

(** Given an arbitrary object, dependencies are resolved like so: *)

let value = [ Some(fun x -> printfn "%d" x; x + 1) ; None ; Some id ]

let assemblies = vmanager.ComputeObjectDependencies(value, permitCompilation = true)

(** An assembly can be exported by generating Vagabond metadata *)

let vagabondAssembly = vmanager.GetVagabondAssembly assembly

(** 

An assembly package contains necessary data to load the specified assembly in a remote process.

Assemblies can be loaded in a remote process like so:

*)

let response = vmanager.LoadVagabondAssembly vagabondAssembly

(**

## Communication

Once all required dependencies have been loaded, communication can be 
established by using the ``.Serializer`` property found in both the server
and client instances. These give an ``FsPickler`` serializer instance that is
capable of serializing and deserializing objects depending on dynamic assemblies.

*)

let serializer = vmanager.Serializer

let bytes = serializer.Pickle(value)