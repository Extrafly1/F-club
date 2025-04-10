open System
// 1-й вариант, ну не придумал я лучше реализацию, сижу уже 3 часа над этим
let parseInput (input: string) =
    input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> 
        match Int32.TryParse(s) with
        | true, num -> Some num
        | _ -> None)
    |> Array.choose id
    |> List.ofArray

let longestCommonSubsequence a b =
    let rec lcs a b =
        match a, b with
        | [], _ -> []
        | _, [] -> []
        | x::xs, y::ys when x = y ->
            x :: lcs xs ys
        | x::xs, y::ys ->
            let lcs1 = lcs xs b
            let lcs2 = lcs a ys
            if List.length lcs1 > List.length lcs2 then lcs1 else lcs2
    lcs a b

printfn "Введите первую последовательность чисел через пробел:"
let list1 = Console.ReadLine() |> parseInput

printfn "Введите вторую последовательность чисел через пробел:"
let list2 = Console.ReadLine() |> parseInput

printfn "\nПервая последовательность: %A" list1
printfn "Вторая последовательность: %A" list2
let result = longestCommonSubsequence list1 list2
printfn "Наибольшая общая подпоследовательность: %A" result
