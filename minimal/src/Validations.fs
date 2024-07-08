namespace MinimalApp

[<RequireQualifiedAccess>]
type ValidIntegerOrString =
    | ValidInteger of int
    | InvalidInteger of int
    | InvalidString of string

[<RequireQualifiedAccess>]
module ValidIntegerOrString =
    let toString (value: ValidIntegerOrString) =
        match value with
        | ValidIntegerOrString.ValidInteger i
        | ValidIntegerOrString.InvalidInteger i -> $"%i{i}"
        | ValidIntegerOrString.InvalidString invalidString -> invalidString

[<RequireQualifiedAccess>]
module Validations =

    let private minDimensionSize = 3
    let private maxDimensionSize = 9

    let private validDimensionRange (dimensionInt: int) =
        let isValid = minDimensionSize <= dimensionInt && dimensionInt <= maxDimensionSize
        printf $"Contains {isValid} - {dimensionInt}"

        if isValid then
            ValidIntegerOrString.ValidInteger dimensionInt
        else
            ValidIntegerOrString.InvalidInteger dimensionInt

    let validDimension (newDimension: string) =
        match newDimension with
        | Utils.StringContainsInt dimensionInt -> validDimensionRange dimensionInt
        | _ -> ValidIntegerOrString.InvalidString newDimension
