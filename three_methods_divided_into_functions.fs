let rec private gcd a b =
    match b with
    | 0 -> abs a
    | _ -> gcd b (a % b)

let isPrime x =
    let rec checkDivisor d =
        match d with
        | d when d * d > x -> true
        | d when x % d = 0 -> false
        | _ -> checkDivisor (d + 1)
    match x with
    | x when x < 2 -> false
    | _ -> checkDivisor 2

let sumDigits n =
    let rec loop x acc =
        match x with
        | 0 -> acc
        | _ -> loop (x / 10) (acc + abs(x) % 10)
    loop (abs n) 0

let processDivisors n initValue operation predicate =
    let rec loop d acc =
        match d with
        | d when d > abs n -> acc
        | d when n % d = 0 -> 
            loop (d + 1) (operation acc (d, predicate d))
        | _ -> loop (d + 1) acc
    loop 1 initValue

// Метод 1: Сумма простых делителей
let sumPrimeDivisors n =
    let folder acc (d, isPrime) = 
        match isPrime with
        | true -> acc + d
        | false -> acc
    processDivisors n 0 folder isPrime

// Метод 2: Количество нечётных цифр > 3
let countOddDigitsOver3 n =
    let rec processDigits num acc =
        match num with
        | 0 -> acc
        | _ ->
            let digit = abs(num) % 10
            processDigits (num / 10) (acc + 
                match digit % 2, digit > 3 with
                | 1, true -> 1
                | _, _ -> 0)
    processDigits (abs n) 0

// Метод 3: Произведение делителей с суммой цифр < исходной
let productSpecialDivisors n =
    let targetSum = sumDigits n
    let predicate d = sumDigits d < targetSum
    let folder acc (d, isValid) =
        match isValid with
        | true -> acc * d
        | false -> acc
    match n with
    | 0 -> 0
    | _ -> processDivisors n 1 folder predicate

[<EntryPoint>]
let main argv =
    printfn "Метод 1: %d" (sumPrimeDivisors 28)
    printfn "Метод 2: %d" (countOddDigitsOver3 496837)
    printfn "Метод 3: %d" (productSpecialDivisors 20)
    0
