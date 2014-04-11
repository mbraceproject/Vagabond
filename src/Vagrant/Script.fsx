//#I "bin/Debug/"

#r "bin/Debug/Mono.Cecil.dll"
#r "bin/Debug/Vagrant.Cecil.dll"

open Nessos.Vagrant.Cecil
open Mono.Cecil
open System.IO

type Foo<'T> = Bar of 'T

let incr (Bar x) = Bar (x + 1)

let a = System.Reflection.Assembly.GetExecutingAssembly()
let parsed = AssemblyParser.Parse a
parsed.Write(Path.Combine(__SOURCE_DIRECTORY__, "bin/Debug/a.dll"))