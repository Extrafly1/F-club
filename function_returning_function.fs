open System

let rec SumNum n =
    match n with
    | 0 -> 0
    | _ -> (abs n) % 10 + SumNum (abs n / 10)

let rec Fact n =
    match n with
    | 1 | 0 -> 1
    | _ -> n * Fact(n - 1)

let switchFun f =
    match f with
    | true -> SumNum
    | false -> Fact

[<EntryPoint>]
let main argv =
    let r = switchFun false
    printfn "%i" (r 10)
    0
