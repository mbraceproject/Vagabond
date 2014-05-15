// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"
//#load "packages/SourceLink.Fake/tools/SourceLink.fsx"
open System
open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.AssemblyInfoFile
//open SourceLink

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "Vagrant"
let authors = ["Nessos Information Technologies, Eirik Tsarpalis"]
let summary = "A library that facilitates the distribution of code in the .NET framework."

let description = summary

let tags = "F# fsharp dynamic code distribution cecil"

let gitHome = "https://github.com/nessos"
let gitName = "Vagrant"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/nessos"


let testAssemblies = [ "bin/Vagrant.Tests.exe" ]

//
//// --------------------------------------------------------------------------------------
//// The rest of the code is standard F# build script 
//// --------------------------------------------------------------------------------------

//// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")
//let isAppVeyorBuild = environVar "APPVEYOR" <> null
//let nugetVersion = 
//    if isAppVeyorBuild then sprintf "%s-a%s" release.NugetVersion (DateTime.UtcNow.ToString "yyMMddHHmm")
//    else release.NugetVersion
//
//Target "BuildVersion" (fun _ ->
//    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" nugetVersion) |> ignore
//)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let vagrantCS = "src/Vagrant.Cecil/Properties/AssemblyInfo.cs"
  CreateCSharpAssemblyInfo vagrantCS
      [ Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion] 

  let vagrantFS = "src/Vagrant/assemblyInfo.fs"
  CreateFSharpAssemblyInfo vagrantFS
      [ Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion] 
)


// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "RestorePackages" (fun _ ->
    !! "./**/packages.config"
    |> Seq.iter (RestorePackage (fun p -> { p with ToolPath = "./.nuget/NuGet.exe" }))
)

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
    let nunitVersion = GetPackageVersion "packages" "NUnit.Runners"
    let nunitPath = sprintf "packages/NUnit.Runners.%s/tools" nunitVersion
    ActivateFinalTarget "CloseTestRunner"

    testAssemblies
    |> NUnit (fun p ->
        { p with
            Framework = "v4.0.30319"
            ToolPath = nunitPath
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
    // Format the description to fit on a single line (remove \r\n and double-spaces)
    let description = description.Replace("\r", "").Replace("\n", "").Replace("  ", " ")
    let nugetPath = ".nuget/NuGet.exe"
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
                    ("FsPickler", "")
                    ("Mono.Cecil", RequireExactly "0.9.5.4")
                ]
            Tags = tags
            OutputPath = "bin"
            ToolPath = nugetPath
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Publish = hasBuildParam "nugetkey" })
        ("nuget/" + project + ".nuspec")
)


Target "Release" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Prepare" DoNothing
Target "PrepareRelease" DoNothing
Target "All" DoNothing

"Clean"
  ==> "RestorePackages"
  ==> "AssemblyInfo"
  ==> "Prepare"
  ==> "Build"
  ==> "RunTests"
  ==> "All"

"All"
  ==> "PrepareRelease" 
  ==> "NuGet"
  ==> "Release"

RunTargetOrDefault "Release"
//RunTargetOrDefault "All"