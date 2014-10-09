(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "FsPickler.dll"
#r "Vagrant.Cecil.dll"
#r "Vagrant.dll"

open System
open System.Reflection

let assembly = Unchecked.defaultof<Assembly>

(**
# API Overview

The included implementation of
[ThunkServer](https://github.com/nessos/Vagrant/blob/master/tests/Vagrant.Tests/ThunkServer.fs) 
is a straightforward distributed implementation that makes use of Vagrant.
What follows is a brief overview of the basic API.

A Vagrant environment can be initialized as follows:

*)

open Nessos.Vagrant

let vagrant = Vagrant.Initialize(cacheDirectory = "/tmp/vagrant")

(** Given an arbitrary object, dependencies are resolved like so: *)

let value = [ Some(fun x -> printfn "%d" x; x + 1) ; None ; Some id ]

let assemblies = vagrant.ComputeObjectDependencies(value, permitCompilation = true)

(** An assembly can be exported by writing *)

let portableAssembly = vagrant.CreatePortableAssembly(assembly, includeAssemblyImage = true)

(** 

A portable assembly contains necessary data to load the specified assembly in a remote process.

Assemblies can be loaded in a remote process like so:

*)

let response : AssemblyLoadInfo = vagrant.LoadPortableAssembly portableAssembly

(**

## Communication

Once all required dependencies have been loaded, communication can be 
established by using the ``.Pickler`` property found in both the server
and client instances. These give an instance of ``FsPickler`` that is
capable of serializing and deserializing objects depending on dynamic assemblies.

*)

let pickler = vagrant.Pickler

let bytes = pickler.Pickle(value)