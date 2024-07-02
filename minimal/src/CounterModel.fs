namespace MinimalApp

type CounterModel = {
    Count: int
}

type CounterMsg =
    | Increment
    | Decrement

module CounterModel =
    let init = {Count = 0}
    
    let update (msg: CounterMsg) (model: CounterModel) =
        match msg with
        | Increment -> {Count = model.Count + 1}
        | Decrement -> {Count = model.Count - 1}
