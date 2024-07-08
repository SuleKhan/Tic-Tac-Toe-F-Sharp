namespace MinimalApp

[<RequireQualifiedAccess>]
type ValidIntegerOrString =
    | ValidInteger of int
    | InvalidInteger of int
    | InvalidString of string

[<RequireQualifiedAccess>]
module Validations =
    // Active pattern
    let (|Int|_|) (input: string) =
        match System.Int32.TryParse input with
        | true, value -> Some value
        | false, _ -> None

    let validDimensionRange (dimensionInt: int) =
        let isValid = 3 <= dimensionInt && dimensionInt < 10

        if isValid then
            ValidIntegerOrString.ValidInteger dimensionInt
        else
            ValidIntegerOrString.InvalidInteger dimensionInt

    let validDimension (newDimension: string) =
        match newDimension with
        | Int dimensionInt -> ValidIntegerOrString.ValidInteger dimensionInt
        | _ -> ValidIntegerOrString.InvalidString newDimension
