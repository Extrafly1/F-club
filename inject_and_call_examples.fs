let rec inject n f acc =
    match n with
    | 0 -> acc
    | _ ->
        let newAcc = f acc (abs(n) % 10)
        inject (n / 10) f newAcc

[<EntryPoint>]
let main argv =
    printfn "%i" (inject 123 (fun x y -> x + y) 0)
    printfn "%i" (inject 123 (fun x y -> if x < y then x else y) 10)
    printfn "%i" (inject 123 (fun x y -> if x < y then y else x) -1)
    printfn "%i" (inject 123 (fun x y -> x * y) 1)
    printfn "%i" (inject 1234567890 (fun x y -> x + 1) 0)
    0
