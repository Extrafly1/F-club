let processCoprimes n (f:int->int->int) acc =
    let rec gcd a b =
        match b with
        | 0 -> abs a
        | _ -> gcd b (a % b)
    
    let rec loop c acc =
        match c with
        | c when c >= (abs n) -> acc
        | _ ->
            let newAcc = 
                if gcd c (abs n) = 1 then f acc c
                else acc
            loop (c+1) newAcc
    
    match n with
    | n when (abs n) < 2 -> acc
    | _ -> loop 1 acc

[<EntryPoint>]
let main argv =
    printfn "%d" (processCoprimes 6 (+) 0)
    printfn "%d" (processCoprimes 9 (*) 1)
    printfn "%d" (processCoprimes 3 (fun acc x -> if acc < x then acc else x) 10)
    printfn "%d" (processCoprimes 7 (fun acc _ -> acc + 1) 0)
    0
