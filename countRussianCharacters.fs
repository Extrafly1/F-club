open System
open System.Text.RegularExpressions

let countRussianCharacters (input: string) =
    let pattern = "[ёа-яЁА-Я]"
    Regex.Matches(input, pattern, RegexOptions.IgnoreCase).Count

printfn "Введите строку для анализа:"
let inputText = Console.ReadLine()

let result = countRussianCharacters inputText
printfn "Количество русских символов: %d" result
