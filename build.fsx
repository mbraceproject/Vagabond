// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"

open System
open System.IO
open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.AssemblyInfoFile

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "Vagabond"

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
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let vagabondCS = "src/Vagabond.AssemblyParser/Properties/AssemblyInfo.cs"
  CreateCSharpAssemblyInfo vagabondCS
      [ Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion] 

  let vagabondFS = "src/Vagabond/AssemblyInfo.fs"
  CreateFSharpAssemblyInfo vagabondFS
      [ Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion] 
)


// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

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
            TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResults.xml" })
)

FinalTarget "CloseTestRunner" (fun _ ->  
    ProcessHelper.killProcess "nunit-agent.exe"
)
//
//// --------------------------------------------------------------------------------------
//// Build a NuGet package

Target "NuGet" (fun _ ->
    Paket.Pack(fun config ->
        { config with 
            Version = release.NugetVersion
            ReleaseNotes = String.concat "\n" release.Notes
            OutputPath = "bin"
            WorkingDir = "nuget"
        }))

Target "NuGetPush" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = "bin/" }))

// Doc generation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    let outputDocsDir = "docs/output"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

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

Target "Prepare" DoNothing
Target "PrepareRelease" DoNothing
Target "Default" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "Prepare"
  ==> "Build"
  ==> "RunTests"
  ==> "Default"

"Build"
  ==> "PrepareRelease"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "NuGet"
  ==> "Release"

"NuGet" 
  ==> "NuGetPush"

RunTargetOrDefault "Default"