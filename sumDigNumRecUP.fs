[<EntryPoint>]
let main argv =
    let rec sumNum n =
        if n = 0 then 0
        else (abs n) % 10 + sumNum (abs n / 10)

    printfn "%i" (sumNum 123545)
    0
