open System
open System.Text.RegularExpressions

let isRussianLetter c =
    Regex.IsMatch(string c, @"^[а-яё]$", RegexOptions.IgnoreCase)

let isVowel c =
    let vowels = set ['а'; 'е'; 'ё'; 'и'; 'о'; 'у'; 'ы'; 'э'; 'ю'; 'я']
    vowels.Contains(Char.ToLower(c))

let countLetters (s: string) =
    s |> Seq.fold (fun (v, c) char ->
        if isRussianLetter char then
            if isVowel char then (v + 1, c) 
            else (v, c + 1)
        else (v, c)
    ) (0, 0)

let calculateDifference s =
    let vowels, consonants = countLetters s
    let total = vowels + consonants
    if total = 0 then 0.0
    else
        let avgV = float vowels / float total
        let avgC = float consonants / float total
        avgC - avgV

printfn "Введите строки (пустая строка для завершения):"
let rec readLines lines =
    let line = Console.ReadLine()
    if String.IsNullOrEmpty(line) then List.rev lines 
    else readLines (line :: lines)

let strings = readLines []
let sortedStrings = strings |> List.sortBy calculateDifference

printfn "\nРезультат сортировки:"
sortedStrings |> List.iter (printfn "%s")
