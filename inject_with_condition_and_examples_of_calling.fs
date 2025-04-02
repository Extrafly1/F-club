let rec injectTwo n (f:int->int->int) acc (p:int->bool) =
    let n = abs n
    match n with
    | 0 -> acc
    | _ ->
        let newAcc = if p n then f acc (n % 10) else acc
        injectTwo (n / 10) f newAcc p

[<EntryPoint>]
let main argv =
    printfn "%i" (injectTwo 123456 (fun x y -> x + y) 0 (fun x -> x % 2 = 0))
    printfn "%i" (injectTwo 123456 (fun x y -> if x < y then x else y) 10 (fun x -> x % 2 = 1))
    printfn "%i" (injectTwo 123456 (fun x y -> x + 1) 0 (fun x -> x % 2 = 0))
    0
