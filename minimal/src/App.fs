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
    | TicTacToe

// MODEL
type Model = {
    Counter: CounterModel
    TicTacToe: TicTacToeModel
    PageId: PageId
}

type Msg =
    | SwitchPage
    | CounterSubMsg of CounterMsg
    | TicTacToeSubMsg of TicTacToeMsg

let init () : Model = {Counter = CounterModel.init; TicTacToe = TicTacToeModel.init ; PageId = PageId.Counter }

// UPDATE
let switchPage (pageId : PageId) =
    match pageId with
    | PageId.Counter -> PageId.TicTacToe
    | PageId.TicTacToe -> PageId.Counter

let update (msg: Msg) (model: Model) =
    match msg with
    | TicTacToeSubMsg subMsg -> {model with TicTacToe = TicTacToeModel.update subMsg model.TicTacToe }
    | CounterSubMsg subMsg -> {model with Counter = CounterModel.update subMsg model.Counter}
    | SwitchPage -> {model with PageId = switchPage model.PageId }

// VIEW (rendered with React)
let view (model: Model) (dispatch: Msg -> unit) : Fable.React.ReactElement =
    Html.main [
        prop.style [
            style.fontFamily "monospace"
            style.fontSize (length.px 16)
        ]
        prop.children [
            match model.PageId with
            | PageId.Counter -> CounterView.view model.Counter (CounterSubMsg >> dispatch)
            | PageId.TicTacToe -> TicTacToeView.view model.TicTacToe (TicTacToeSubMsg >> dispatch)
            Html.button [
                prop.style [
                    style.margin (length.em 1)
                ]
                prop.onClick(fun _ -> dispatch SwitchPage)
                prop.text "Switch Page"
            ]
        ]
    ]
    

// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.withDebugger
|> Program.run
