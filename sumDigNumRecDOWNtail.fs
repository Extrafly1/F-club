[<EntryPoint>]
let main argv =
    let sumNum n =
        let rec sumNumRec n acc =
            if n = 0 then acc
            else sumNumRec (n / 10) (acc + n % 10)
        sumNumRec (abs n) 0

    printf "%i" (sumNum 123545)
    0