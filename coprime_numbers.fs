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

let eulerPhi n =
    match n with
    | n when (abs n) = 1 -> 1
    | _ -> processCoprimes (abs n) (fun acc _ -> acc + 1) 0

let processCoprimesWith n (f:int->int->int) i (y:int->bool) =
    let rec gcd a b =
        match b with
        | 0 -> abs a
        | _ -> gcd b (a % b)
    
    let rec loop c acc =
        match c with
        | c when c >= abs n -> acc
        | _ ->
            let newAcc = 
                if gcd c (abs n) = 1 && y c 
                then f acc c 
                else acc
            loop (c + 1) newAcc
    
    match n with
    | n when (abs n) < 2 -> i
    | _ -> loop 1 i

[<EntryPoint>]
let main argv =
    printfn "%d" (processCoprimes 6 (+) 0)
    printfn "%d" (processCoprimes 9 (*) 1)
    printfn "%d" (processCoprimes 3 (fun acc x -> if acc < x then acc else x) 10)
    printfn "%d" (processCoprimes 7 (fun acc _ -> acc + 1) 0)
    printfn "1 -> %d" (eulerPhi 1)
    printfn "7 -> %d" (eulerPhi 7)
    printfn "9 -> %d" (eulerPhi 9)
    printfn "6 -> %d" (eulerPhi 6)
    printfn "0 -> %d" (eulerPhi 0)
    printfn "%d" (processCoprimesWith 10 (+) 0 (fun x -> x % 2 = 0))
    printfn "%d" (processCoprimesWith 15 (fun acc _ -> acc + 1) 0 (fun x -> x > 5))
    printfn "%d" (processCoprimesWith 9 (fun acc x -> min acc x) 10 (fun x -> x % 2 = 0))
    printfn "%d" (processCoprimesWith 7 (+) 0 (fun x -> x < 5))
    0
