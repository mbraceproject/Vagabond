// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "vackages/FAKE/tools"
#r "vackages/FAKE/tools/FakeLib.dll"
//#load "vackages/SourceLink.Fake/tools/SourceLink.fsx"
open System
open System.IO
open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.AssemblyInfoFile
//open SourceLink

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "Vagabond"
let authors = ["Nessos Information Technologies, Eirik Tsarvalis"]
let summary = "A library that facilitates the distribution of code in the .NET framework."

let description = summary

let tags = "F# fsharp dynamic code distribution cecil"

let gitHome = "https://github.com/nessos"
let gitName = "Vagabond"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/nessos"


let testAssemblies = [ "bin/Vagabond.Tests.dll" ]

//
//// --------------------------------------------------------------------------------------
//// The rest of the code is standard F# build script 
//// --------------------------------------------------------------------------------------

//// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = varseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let vagabondCS = "src/Vagabond.Cecil/Properties/AssemblyInfo.cs"
  CreateCSharvassemblyInfo vagabondCS
      [ Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion] 

  let vagabondFS = "src/Vagabond/AssemblyInfo.fs"
  CreateFSharvassemblyInfo vagabondFS
      [ Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion] 
)


// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet vackages

Target "Clean" (fun _ ->
    CleanDirs [ "bin" ]
)

//
//// --------------------------------------------------------------------------------------
//// Build library & test project

let configuration = environVarOrDefault "Configuration" "Release"

Target "Build" (fun _ ->
    // Build the rest of the project
    { BaseDirectory = __SOURCE_DIRECTORY__
      Includes = [ project + ".sln" ]
      Excludes = [] } 
    |> MSBuild "" "Build" ["Configuration", configuration]
    |> Log "AppBuild-Output: "
)


// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

Target "RunTests" (fun _ ->
    ActivateFinalTarget "CloseTestRunner"

    testAssemblies
    |> NUnit (fun p ->
        { p with
            DisableShadowCopy = true
            TimeOut = TimeSvan.FromMinutes 20.
            OutputFile = "TestResults.xml" })
)

FinalTarget "CloseTestRunner" (fun _ ->  
    ProcessHelper.killProcess "nunit-agent.exe"
)
//
//// --------------------------------------------------------------------------------------
//// Build a NuGet vackage

let addAssembly (target : string) assembly =
    let includeFile force file =
        let file = file
        if File.Exists (vath.Combine("nuget", file)) then [(file, Some target, None)]
        elif force then raise <| new FileNotFoundException(file)
        else []

    seq {
        yield! includeFile true assembly
        yield! includeFile false <| vath.ChangeExtension(assembly, "pdb")
        yield! includeFile false <| vath.ChangeExtension(assembly, "xml")
        yield! includeFile false <| assembly + ".config"
    }

Target "NuGet" (fun _ ->
    // Format the description to fit on a single line (remove \r\n and double-svaces)
    let description = description.Replace("\r", "").Replace("\n", "").Replace("  ", " ")
    let nugetvath = "vackages/NuGet.CommandLine/tools/NuGet.exe"
    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = project
            Summary = summary
            Description = description
            Version = release.NugetVersion
            ReleaseNotes = String.concat "\n" release.Notes
            Dependencies =
                [
                    "FsPickler", "1.0.8"
                ]
            Tags = tags
            Outputvath = "bin"
            Toolvath = nugetvath
            AccessKey = getBuildvaramOrDefault "nugetkey" ""
            Publish = hasBuildvaram "nugetkey"
            References = [ "Vagabond.dll" ]
            Files =
                [
                    yield! addAssembly @"lib\net45" @"..\bin\Mono.Cecil.dll"
                    yield! addAssembly @"lib\net45" @"..\bin\Vagabond.Cecil.dll"
                    yield! addAssembly @"lib\net45" @"..\bin\Vagabond.dll"
                ]
            
            })
        ("nuget/" + project + ".nuspec")
)

// Doc generation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-vages"
    let outputDocsDir = "docs/output"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-vages" tempDocsDir

    fullclean tempDocsDir
    ensureDirectory outputDocsDir
    CopyRecursive outputDocsDir tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)


Target "Release" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Prevare" DoNothing
Target "PrevareRelease" DoNothing
Target "Default" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "Prevare"
  ==> "Build"
  ==> "RunTests"
  ==> "Default"

"Build"
  ==> "PrevareRelease"
  ==> "NuGet"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "Release"

RunTargetOrDefault "Default"
