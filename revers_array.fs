open System

let reverseArray arr = Array.rev arr

printfn "Введите строку для обращения:"
let inputString = Console.ReadLine()
let charArray = inputString.ToCharArray()

let reversedArray = reverseArray charArray
let reversedString = String(reversedArray)

printfn "\nИсходная строка: %s" inputString
printfn "Массив символов: %A" charArray
printfn "Обращенный массив: %A" reversedArray
printfn "Результирующая строка: %s" reversedString
