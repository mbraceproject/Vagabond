module internal Nessos.Vagabond.Flow

open System
open System.IO
open System.Reflection

open Mono.Cecil

open Nessos.Vagabond.AssemblyParser
open Nessos.Vagabond.SliceCompilerTypes


//let rec computeTypeFieldDependencies (state:DynamicAssemblyState) (erased : FieldInfo[]) (typeDef:TypeDefinition) =
//    typeDef.Methods |> Seq.filter (fun m -> not m.IsConstructor && not m.IsStatic)
//
//and computeMethodDependencies (state:DynamicAssemblyState) (erased : FieldInfo[]) (m:MethodDefinition) =
//
//let computeAssemblyFieldDependencies (state:DynamicAssemblyCompilerState) (erased : FieldInfo []) (assembly:AssemblyDefinition) : Map<string, FieldInfo []> =
//    let typeDefs = assembly.Modules |> Seq.collect(fun m -> m.Types)
//
//    failwith ""
//    let typeDef = assembly.GetType
//    for m in typeDef.
//    let getTypeDefinition (assembly:AssemblyDefinition) (t : Type) =
//        assembly.Mo
//        assembly.Modules |> Seq.collect (fun m -> m.GetT)
    
//    assembly.MainModule.GetT
    