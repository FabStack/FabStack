module Index

open Fable.Remoting.Client
open Shared

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser
open Elmish
open Thoth.Json

open Feliz
open Feliz.ReactEditor
open Feliz.Molstar

open Fetch

let fetchContent pdbId =
    async {
        let! response = fetch (sprintf "https://files.rcsb.org/download/%s.cif" pdbId) [] |> Async.AwaitPromise
        let! item = response.text() |> Async.AwaitPromise
        return item
    }

type Model = 
    { 
        Todos: Todo list
        Input: string 
        PdbIdInput: string
        EditorValue: string
        RawPdbText: string
    }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo
    | ChangeEditorValue of string
    | ChangePdbIdInput of string
    | SetPdbId
    | FetchedContent of string
    | SetRawPdbText of string

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () =
    let model = 
        { 
            Todos = [] 
            Input = ""
            EditorValue = "" 
            PdbIdInput = "1LOL"
            RawPdbText = ""
        }
    let cmd = Cmd.OfAsync.perform todosApi.getTodos () GotTodos
    model, cmd

let update msg model =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input
        let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo
        { model with Input = "" }, cmd
    | AddedTodo todo ->
        { model with Todos = model.Todos @ [ todo ] }, Cmd.none
    | ChangeEditorValue s -> { model with EditorValue = s }, Cmd.none
    | ChangePdbIdInput s -> { model with PdbIdInput = s }, Cmd.none
    | FetchedContent s -> { model with EditorValue = s }, Cmd.none
    | SetPdbId -> 
        let cmd = Cmd.OfAsync.perform fetchContent model.PdbIdInput FetchedContent
        model, cmd
    | SetRawPdbText s -> { model with RawPdbText = s }, Cmd.none

let view model dispatch =

    let options = jsOptions<Monaco.Editor.IEditorConstructionOptions>(fun (o: Monaco.Editor.IEditorConstructionOptions) ->
            o.minimap <- Some (jsOptions(fun oMinimap -> oMinimap.enabled <- Some false))
            o.theme <- Some "vs-dark"
        )

    Html.section [
        prop.className "flex flex-col h-screen"
        prop.children [
            Html.div [
                prop.className "flex flex-row bg-gray-200 p-4"
                prop.children [
                    Html.input [
                        prop.value model.PdbIdInput
                        prop.onChange (fun (s : string) -> s |> ChangePdbIdInput |> dispatch)
                    ]
                    Html.button [
                        prop.className "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded ml-2"
                        prop.onClick (fun _ -> SetPdbId |> dispatch)
                        prop.children [ Html.text "Click to retrieve raw PDB source by ID" ]
                    ]
                    Html.button [
                        prop.className "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded ml-2"
                        prop.onClick (fun _ -> SetRawPdbText model.EditorValue |> dispatch)
                        prop.children [ Html.text "Click to send editor value to viewer" ]
                    ]
                ]
            ]
            Html.div [
                prop.className "flex flex-row h-full w-full"
                prop.children [
                    Html.div [
                        prop.style [
                            style.width (length.perc 50)
                            style.height (length.perc 100)
                        ]
                        prop.children [
                            ReactEditor.editor [
                                editor.options options
                                editor.value (model.EditorValue)
                                editor.onChange (fun (s : string) -> s |> ChangeEditorValue |> dispatch)
                            ]
                        ]
                    ]
                    Html.div [
                        prop.style [
                            style.width (length.perc 50)
                            style.height (length.perc 100)
                        ]
                        prop.children [
                            Molstar.molstar [ molstar.rawPdbText model.RawPdbText ]
                        ]
                    ]
                ]
            ]
        ]
    ]