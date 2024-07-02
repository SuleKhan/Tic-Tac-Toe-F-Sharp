module MinimalApp.App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Elmish.Debug
open Feliz


[<RequireQualifiedAccess>]
type PageId =
    | Counter
    | About

// MODEL
type Model = {
    Counter: CounterModel
    About: string
    PageId: PageId
}



type Msg =
    | SwitchPage
    | CounterSubMsg of CounterMsg

let init () : Model = {Counter = CounterModel.init; About = "About"; PageId = PageId.Counter }

// UPDATE

let switchPage (pageId : PageId) =
    match pageId with
    | PageId.Counter -> PageId.About
    | PageId.About -> PageId.Counter



let update (msg: Msg) (model: Model) =
    match msg with
    | CounterSubMsg subMsg -> {model with Counter = CounterModel.update subMsg model.Counter}
    | SwitchPage -> {model with PageId = switchPage model.PageId }

// VIEW (rendered with React)

    
let aboutView (about: string) =
    Html.div [
        Html.div [ prop.text about ]
    ]

let view (model: Model) (dispatch: Msg -> unit) : Fable.React.ReactElement =
    Html.div [
        match model.PageId with
        | PageId.Counter -> CounterView.view model.Counter (CounterSubMsg >> dispatch)
        | PageId.About -> aboutView model.About
        Html.button [
            prop.onClick(fun _ -> dispatch SwitchPage)
            prop.text "Switch Page"
        ]
    ]
    

// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.withDebugger
|> Program.run
