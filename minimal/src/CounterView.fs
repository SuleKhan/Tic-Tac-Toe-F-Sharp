module MinimalApp.CounterView

open Feliz

let view (model: CounterModel) (dispatch: CounterMsg -> unit) : Fable.React.ReactElement =
    Html.div [
        Html.button [
            prop.onClick(fun _ -> dispatch Increment)
            prop.text "+"
        ]
        Html.div [ prop.text (string<int> model.Count) ]
        Html.button [
            prop.onClick(fun _ -> dispatch Decrement)
            prop.text "-"
        ]
    ]
