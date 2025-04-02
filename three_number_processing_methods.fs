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

// Метод 2: Количество нечетных цифр > 3
let countOddDigitsOver3 n =
    let rec loop num acc =
        match num with
        | 0 -> acc
        | _ ->
            let digit = abs(num) % 10
            loop (num / 10) (acc + 
                match digit % 2, digit > 3 with
                | 1, true -> 1
                | _, _ -> 0)
    loop (abs n) 0

// Метод 3: Произведение делителей с суммой цифр < исходной
let productSpecialDivisors n =
    let rec sumDigits x acc =
        match x with
        | 0 -> acc
        | _ -> sumDigits (x / 10) (acc + abs(x) % 10)
    
    let target = sumDigits (abs n) 0
    
    let rec checkDivisor d acc =
        match d with
        | d when d > abs n -> acc
        | d when n % d = 0 ->
            let divSum = sumDigits d 0
            checkDivisor (d + 1) (acc * match divSum < target with | true -> d | false -> 1)
        | _ -> checkDivisor (d + 1) acc
    
    match n with
    | 0 -> 0
    | _ -> checkDivisor 1 1

[<EntryPoint>]
let main argv =
    printfn "Метод 1: %i (28 -> 2+7=9)" (sumPrimeDivisors 28)
    printfn "Метод 2: %i (496837 -> 9,7 → 2)" (countOddDigitsOver3 496837) 
    printfn "Метод 3: %i (20 -> 1*10=10)" (productSpecialDivisors 20) 
    0
