# Vagrant

A library that facilitates the distribution of code in the .NET framework. 

## Introduction

On-demand distribution of code (e.g. lambdas) for remote execution is a problem
often exacerbated by the presence of dynamic assemblies. Dynamic assemblies 
appear in situations where code is emitted at runtime, such interpreters,
optimization libraries or F# type providers. By default, dynamic assemblies
are not persistable. What's more, the inherently incremental nature of dynamic
assemblies makes their exportation even more challenging.

Vagrant attempts to solve this problem by providing an automated dependency resolution
and dynamic assembly compilation framework. It also addresses issues inherent
in REPL loops. It is based on the `Mono.Cecil` and `FsPickler` libraries.

## Demo

To see a demo of Vagrant, build the project and execute the `thunkServer.fsx` script found
inside the samples folder. As the name suggests, it allows for execution of arbitrary thunks
in a remote server.

## Overview

The included implementation of ThunkServer is a straightforward example of the Vagrant API.
A brief overview follows:

A code export environment can be initialized as follows:
```fsharp
open Nessos.Vagrant

let server = new VagrantServer()
```
Given an arbitrary object, dependencies are resolved like so:
```fsharp
let obj = Some(fun x -> printfn "%d" x; x + 1) :> obj

let assemblies : Assembly list = server.ComputeObjectDependencies(obj, permitCompilation = true)
```
An assembly can be exported by writing
```fsharp
let portableAssembly : PortableAssembly = vagrant.MakePortableAssembly(assembly, includeAssemblyImage = true)
```
A portable assembly contains necessary data to load the specified assembly in a remote process.

On the client side, assemblies can be loaded like so:
```fsharp
let client = new VagrantClient()

let response : AssemblyLoadResponse = client.LoadPortableAssembly(portableAssembly)
```
To transparently submit all required dependencies of an object:
```fsharp
do server.SubmitObjectDependencies(submitF, obj, permitCompilation = true)
```
where ``submitF : PortableAssembly list -> Async<AssemblyLoadResponse list>`` is
a user-provided dependency transport implementation.