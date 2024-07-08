module MinimalApp.TicTacToeView

open System
open Browser.Types
open Feliz


let victoryText (victor: Player) =
    match victor with
    | Player.X -> "Player X Wins"
    | Player.O -> "Player O Wins"

let gameOverText (gameState: GameState) =
    match gameState with
    | Victory victor -> victoryText victor
    | Draw -> "Game is a Draw"
    | InProgress -> ""

let gameOver (gameState: GameState) =
    Html.div [ prop.text (gameOverText gameState) ]

let private buttonLabel (coord: Coord) (spaces: Map<Coord, Player>) =
    if spaces.ContainsKey coord then
        match spaces[coord] with
        | Player.X -> "X"
        | Player.O -> "O"
    else
        "-"

let private boardButton (coord: Coord) (model: TicTacToeModel) dispatch =
    Html.button [
        prop.style [
            style.display.inlineBlock
            style.fontSize (length.em 1)
            style.width (length.em 2)
            style.height (length.em 2)
            style.textAlign.center
        ]
        prop.onClick (fun _ -> dispatch (SelectSpace (model.Turn, coord)))
        prop.text (buttonLabel coord model.Board.Spaces)
    ]

let private boardCell (row: int) (col: int) (model: TicTacToeModel) dispatch =
    Html.div [
        prop.style [
            style.gridRowStart row
            style.gridColumnStart col
        ]
        prop.children [
            boardButton (Coord.init (row - 1) (col - 1)) model dispatch
        ]
    ]

let renderBoard (model: TicTacToeModel) dispatch =
    Html.div [
        prop.style [ style.display.inlineBlock ]
        prop.children [
            Html.div [
                prop.style [
                    style.display.grid
                    style.gridTemplateColumns (model.Board.Dimensions.Width, length.em 2)
                    style.gridTemplateRows (model.Board.Dimensions.Height, length.em 2)
                    style.padding (length.em 1)
                ]
                prop.children [
                    for row in [ 1 .. model.Board.Dimensions.Height ] do
                        for col in [ 1 .. model.Board.Dimensions.Width ] do
                            boardCell row col model dispatch
                ]
            ]
        ]
    ]
// type DimensionToUpdate =
//     | Width
//     | Height

[<RequireQualifiedAccess>]
type TextInputValidity =
    | Valid
    | Invalid of Message: string

type TextInputProps = {
    InputLabel: string
    // MinSize: int
    Value: string
    OnChange: string -> unit
    Validity: TextInputValidity
// DimensionToUpdate: DimensionToUpdate
}

// let dimensionInputValue (dimension: DimensionToUpdate) (newDimension: Dimensions) =
//     match dimension with
//     | Width -> newDimension.Width
//     | Height -> newDimension.Height

// let getNewDimensions (newDimensions: Result<Dimensions, DimensionsError>) =
//     match newDimensions with
//     | Ok dimensions -> dimensions
//     | Error errorDimensions -> errorDimensions.ErrorDimensions
//
// let dimensionInputOnChange (newSize: int) (dimensionToUpdate: DimensionToUpdate) (newDimensions: Result<Dimensions, DimensionsError>) dispatch =
//     match dimensionToUpdate with
//     | Width -> dispatch (SetNewWidth {getNewDimensions newDimensions with Width = newSize})
//     | Height -> dispatch (UpdateDimensions {getNewDimensions newDimensions with Height = newSize})

let renderDimensionInput (dimensionInput: TextInputProps) (model: TicTacToeModel) dispatch =
    Html.div [
        prop.style [ style.padding (length.em 1) ]
        prop.children [
            Html.div [ prop.text dimensionInput.InputLabel ]
            Html.input [
                prop.type' "number"
                // prop.min dimensionInput.MinSize
                // prop.value (dimensionInputValue dimensionInput.DimensionToUpdate model.NewDimensions)
                // prop.onChange(fun (newSize: int) -> dimensionInputOnChange newSize dimensionInput.DimensionToUpdate model.NewDimensions dispatch)
                prop.value dimensionInput.Value
                prop.onChange dimensionInput.OnChange
            ]
            match dimensionInput.Validity with
            | TextInputValidity.Valid -> ()
            | TextInputValidity.Invalid errorMessage ->
                Html.div [
                    prop.style [ style.color "red" ]
                    prop.text errorMessage
                ]
        ]
    ]

// let errorMessage (model: TicTacToeModel) =
//     let errorText =
//         match model.NewDimensions with
//         | Ok _ -> ""
//         | Error errorDimensions -> errorDimensions.ErrorMessage
//
//     Html.div [ prop.style [ style.color "red" ]; prop.text errorText ]

let resetButton dispatch =
    Html.button [
        prop.style [ style.margin 10 ]
        prop.onClick (fun _ -> dispatch ResetGame)
        prop.text "Reset Board"
    ]

let view (model: TicTacToeModel) (dispatch: TicTacToeMsg -> unit) : Fable.React.ReactElement =
    let dimensionToValidity (value: ValidIntegerOrString) =
        match value with
        | ValidIntegerOrString.ValidInteger _ -> TextInputValidity.Valid
        | ValidIntegerOrString.InvalidInteger _ ->
            TextInputValidity.Invalid "Please choose a width and height between 3 and 9."
        | ValidIntegerOrString.InvalidString _ -> TextInputValidity.Invalid "Please input an number."

    let widthDimensionInput = {
        InputLabel = "Board Width"
        Value = ValidIntegerOrString.toString model.NewWidth
        OnChange = TicTacToeMsg.SetNewWidth >> dispatch
        Validity = dimensionToValidity model.NewWidth
    }

    let heightDimensionInput = {
        InputLabel = "Board Height"
        Value = ValidIntegerOrString.toString model.NewHeight
        OnChange = TicTacToeMsg.SetNewHeight >> dispatch
        Validity = dimensionToValidity model.NewHeight
    }

    Html.div [
        gameOver model.GameState
        renderBoard model dispatch
        Html.div [
            prop.style [ style.display.flex ]
            prop.children [
                renderDimensionInput widthDimensionInput model dispatch
                renderDimensionInput heightDimensionInput model dispatch
            ]
        ]
        resetButton dispatch
    ]

// px (specifying font-size on root of doc), percent (only font-size), em (always, unless font-size), rem, vh, vw
