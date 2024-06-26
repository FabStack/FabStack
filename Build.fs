open Fake.Core
open Fake.IO
open Farmer
open Farmer.Builders

open Helpers

initializeContext ()

let clientPath = Path.getFullName "src/Client"
let deployPath = Path.getFullName "deploy"
let clientTestsPath = Path.getFullName "tests/Client"

Target.create "Clean" (fun _ ->
    Shell.cleanDir deployPath
    run dotnet [ "fable"; "clean"; "--yes" ] clientPath // Delete *.fs.js files created by Fable
)

Target.create "InstallClient" (fun _ -> run npm [ "install" ] ".")

Target.create "Bundle" (fun _ ->
    [
        "client", dotnet [ "fable"; "-o"; "output"; "-s"; "--run"; "npx"; "vite"; "build" ] clientPath
    ]
    |> runParallel)

Target.create "Azure" (fun _ ->
    let web = webApp {
        name "SAFE-App"
        operating_system OS.Linux
        runtime_stack (DotNet "8.0")
        zip_deploy "deploy"
    }

    let deployment = arm {
        location Location.WestEurope
        add_resource web
    }

    deployment |> Deploy.execute "SAFE-App" Deploy.NoParameters |> ignore)

Target.create "Run" (fun _ ->
    [
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "--run"; "npx"; "vite" ] clientPath
    ]
    |> runParallel)

Target.create "RunTests" (fun _ ->
    [
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "--run"; "npx"; "vite" ] clientTestsPath
    ]
    |> runParallel)

Target.create "Format" (fun _ -> run dotnet [ "fantomas"; "." ] ".")

open Fake.Core.TargetOperators

let dependencies = [
    "Clean" ==> "InstallClient" ==> "Bundle" ==> "Azure"

    "Clean" ==> "InstallClient" ==> "Run"

    "InstallClient" ==> "RunTests"
]

[<EntryPoint>]
let main args = runOrDefault args