[<RequireQualifiedAccess>]
module MinimalApp.Utils

// Active pattern
let (|StringContainsInt|_|) (input: string) =
    match System.Int32.TryParse input with
    | true, value -> Some value
    | false, _ -> None
