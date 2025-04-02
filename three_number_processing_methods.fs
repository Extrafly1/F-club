// 21 mod 10 = 1 => Вариант 1
// Метод 1: Сумма простых делителей
let sumPrimeDivisors n =
    let rec isPrime x d =
        match d with
        | d when d * d > x -> true
        | d when x % d = 0 -> false
        | _ -> isPrime x (d + 1)
    
    let rec loop divisor acc =
        match divisor with
        | d when d > abs n -> acc
        | d when n % d = 0 -> 
            match d with
            | 1 -> loop (d + 1) acc
            | _ -> loop (d + 1) (acc + match isPrime d 2 with | true -> d | false -> 0)
        | _ -> loop (divisor + 1) acc
    
    match n with
    | 0 -> 0
    | _ -> loop 1 0

[<EntryPoint>]
let main argv =
    printfn "Метод 1: %i (28 -> 2+7=9)" (sumPrimeDivisors 28)
    0
