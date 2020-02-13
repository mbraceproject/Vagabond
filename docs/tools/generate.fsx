// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docs/content' directory
// (the generated documentation is stored in the 'docs/output' directory)
// --------------------------------------------------------------------------------------
#I "../../packages/docgeneration/FSharp.Compiler.Service/lib/netstandard2.0"
#I "../../packages/docgeneration/FSharp.Formatting/lib/netstandard2.0"
#r "../../packages/docgeneration/FAKE/tools/FakeLib.dll"
#r "RazorEngine.NetCore.dll"
#r "FSharp.Compiler.Service.dll"
#r "FSharp.Markdown.dll"
#r "FSharp.Literate.dll"
#r "FSharp.CodeFormat.dll"
#r "FSharp.MetadataFormat.dll"
#r "FSharp.Formatting.Common.dll"
#r "FSharp.Formatting.Razor.dll"

open System
open System.IO
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open FSharp.Formatting.Razor

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

// Binaries that have XML documentation (in a corresponding generated XML file)
let referenceProjects = [ "../../src/Vagabond" ]
// Web site location for the generated documentation
let website = "/Vagabond"

let githubLink = "http://github.com/mbraceproject/Vagabond"

// Specify more information about your project
let info =
  [ "project-name", "Vagabond"
    "project-author", "Eirik Tsarpalis"
    "project-summary", "A library for distributing dynamic assemblies"
    "project-github", githubLink
    "project-nuget", "http://www.nuget.org/packages/Vagabond" ]

// --------------------------------------------------------------------------------------
// For typical project, no changes are needed below
// --------------------------------------------------------------------------------------

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ @@ "../output")
#endif

// paths with template/source/output locations
let content    = __SOURCE_DIRECTORY__ @@ "../content"
let output     = __SOURCE_DIRECTORY__ @@ "../output"
let files      = __SOURCE_DIRECTORY__ @@ "../files"
let templates  = __SOURCE_DIRECTORY__ @@ "templates"
let formatting = __SOURCE_DIRECTORY__ @@ "../../packages/docgeneration/FSharp.Formatting/"
let docTemplate = formatting @@ "templates/docpage.cshtml"

// Where to look for *.csproj templates (in this order)
let layoutRoots =
  [ templates; formatting @@ "templates"
    formatting @@ "templates/reference" ]

// Copy static files and CSS + JS from F# Formatting
let copyFiles () =
    Fake.IO.DirectoryInfo.copyRecursiveTo true (DirectoryInfo output) (DirectoryInfo files) |> ignore
    Fake.IO.Directory.ensure (output @@ "content")
    Fake.IO.DirectoryInfo.copyRecursiveTo true (DirectoryInfo (output @@ "content")) (DirectoryInfo (formatting @@ "styles")) |> ignore


let getReferenceAssembliesForProject (proj : string) =
    let projName = Path.GetFileName proj
    !! (proj @@ "bin/Release/netcoreapp*/" + projName + ".dll") |> Seq.head

// Build API reference from XML comments
let buildReference () =
  Shell.cleanDir (output @@ "reference")
  let binaries = referenceProjects |> List.map getReferenceAssembliesForProject
  RazorMetadataFormat.Generate
    ( binaries, output @@ "reference", layoutRoots, 
      parameters = ("root", root)::info,
      sourceRepo = githubLink @@ "tree/master",
      sourceFolder = __SOURCE_DIRECTORY__ @@ ".." @@ "..",
      publicOnly = true )

// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =
  let subdirs = Directory.EnumerateDirectories(content, "*", SearchOption.AllDirectories)
  for dir in Seq.append [content] subdirs do
    let sub = if dir.Length > content.Length then dir.Substring(content.Length + 1) else "."
    RazorLiterate.ProcessDirectory
      ( dir, docTemplate, output @@ sub, replacements = ("root", root)::info,
        layoutRoots = layoutRoots, generateAnchors = true )

// Generate
Shell.cleanDir output
Shell.mkdir output
copyFiles()
buildDocumentation()
buildReference()
