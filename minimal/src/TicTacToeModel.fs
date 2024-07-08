namespace MinimalApp

open Microsoft.FSharp.Core


[<RequireQualifiedAccess>]
type Player =
    | X
    | O

[<RequireQualifiedAccess>]
module Player =
    let otherPlayer (player: Player) =
        match player with
        | Player.X -> Player.O
        | Player.O -> Player.X

type GameState =
    | Victory of Player
    | Draw
    | InProgress

type Coord = { Row: int; Col: int }

[<RequireQualifiedAccess>]
module Coord =
    let init (row: int) (col: int) = { Row = row; Col = col }

type Dimensions = { Width: int; Height: int }

module Dimensions =
    let init (width: int) (height: int) = { Width = width; Height = height }

type DimensionsError = {
    ErrorDimensions: Dimensions
    ErrorMessage: string
}

type Board = {
    Dimensions: Dimensions
    Spaces: Map<Coord, Player>
}

[<RequireQualifiedAccess>]
module Board =
    let init (dimensions: Dimensions) = {
        Dimensions = dimensions
        Spaces = Map.empty
    }

    let containsCoord (coord: Coord) (board: Board) =
        0 <= coord.Row
        && coord.Row < board.Dimensions.Height
        && 0 <= coord.Col
        && coord.Col < board.Dimensions.Width

    let isFilled (board: Board) =
        let filledSpaces =
            board.Spaces
            |> Map.filter (fun coord _ -> containsCoord coord board)
            |> Map.count

        filledSpaces = board.Dimensions.Width * board.Dimensions.Height

type TicTacToeModel = {
    Board: Board
    NewWidth: ValidIntegerOrString
    NewHeight: ValidIntegerOrString
    Turn: Player
    GameState: GameState
}

type TicTacToeMsg =
    | SelectSpace of Player * Coord
    | ResetGame
    | SetNewWidth of string
    | SetNewHeight of string

module TicTacToeModel =
    let defaultDimension = 3
    let newDimensions = Dimensions.init defaultDimension defaultDimension

    let init = {
        Board = Board.init newDimensions
        NewWidth = ValidIntegerOrString.ValidInteger defaultDimension
        NewHeight = ValidIntegerOrString.ValidInteger defaultDimension
        Turn = Player.X
        GameState = InProgress
    }

    let private flip f a b = f b a

    let private changeTurn (model: TicTacToeModel) =
        let nextPlayer = Player.otherPlayer model.Turn
        { model with Turn = nextPlayer }

    let checkSpacesWinner (coords: Coord List) (board: Board) =
        let spaces = List.map board.Spaces.TryFind coords
        let head = List.tryHead spaces |> Option.flatten
        let headHasWon = List.forall ((=) head) spaces
        if headHasWon then head else None

    let private checkRowWinner (row: int) (board: Board) =
        let coords = List.init board.Dimensions.Width (Coord.init row)
        checkSpacesWinner coords board

    let private checkColWinner (col: int) (board: Board) =
        let coords = List.init board.Dimensions.Height (flip Coord.init col)
        checkSpacesWinner coords board

    let checkDiagsWinner (board: Board) =
        let applyTwice f a = f a a

        let rank = min board.Dimensions.Width board.Dimensions.Height
        let leftDiagCoords = List.init rank (applyTwice Coord.init)
        let leftDiagWinner = checkSpacesWinner leftDiagCoords board

        let rightDiagCoords =
            List.init rank (fun i -> Coord.init i (board.Dimensions.Width - i - 1))

        let rightDiagWinner = checkSpacesWinner rightDiagCoords board

        match leftDiagWinner, rightDiagWinner with
        | Some winner, _
        | _, Some winner -> Some winner
        | None, None -> None

    let private checkWinner (board: Board) =

        let rowWinner =
            List.init board.Dimensions.Height (flip checkRowWinner board)
            |> List.tryPick id

        let colWinner =
            List.init board.Dimensions.Width (flip checkColWinner board)
            |> List.tryPick id

        let diagWinner = checkDiagsWinner board

        match rowWinner, colWinner, diagWinner with
        | Some winner, _, _
        | _, Some winner, _
        | _, _, Some winner -> Some winner
        | None, None, None -> None


    let updateGameState (model: TicTacToeModel) =

        match checkWinner model.Board, Board.isFilled model.Board with
        | Some winner, _ -> {
            model with
                GameState = Victory winner
          }
        | None, true -> { model with GameState = Draw }
        | None, false -> model

    let private setPlayerSpace (player: Player) (coord: Coord) (model: TicTacToeModel) =
        let newSpaces = model.Board.Spaces.Add (coord, player)
        let newBoard = { model.Board with Spaces = newSpaces }
        { model with Board = newBoard }

    let private selectSpace (player: Player) (coord: Coord) (model: TicTacToeModel) =
        if model.Board.Spaces.ContainsKey coord then
            model
        else
            model
            |> setPlayerSpace player coord
            |> changeTurn
            |> updateGameState

    let resetGame (model: TicTacToeModel) =
        let newBoard =
            match model.NewWidth, model.NewHeight with
            | ValidIntegerOrString.ValidInteger newWidth, ValidIntegerOrString.ValidInteger newHeight ->
                let newDimensions = { Width = newWidth; Height = newHeight }
                Board.init newDimensions
            | _, _ -> model.Board

        {
            Board = newBoard
            NewHeight = model.NewHeight
            NewWidth = model.NewWidth
            Turn = Player.X
            GameState = InProgress
        }

    let update (msg: TicTacToeMsg) (model: TicTacToeModel) =
        match msg with
        | SelectSpace (player, coord) ->
            if model.GameState = InProgress then
                selectSpace player coord model
            else
                model
        | ResetGame -> resetGame model
        | SetNewWidth newWidthStr ->
            let newWidth = Validations.validDimension newWidthStr
            { model with NewWidth = newWidth }
        | SetNewHeight newHeightStr ->
            let newHeight = Validations.validDimension newHeightStr
            { model with NewHeight = newHeight }
