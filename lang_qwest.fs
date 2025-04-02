open System

let generateResponse (lang:string) =
    let l = lang.ToLower()
    match l with
    | "f#" | "prolog" -> "Ты — подлиза!"
    | "python" -> "Змеиный язык? Серьёзно?"
    | "java" -> "Кофеёк подождёт..."
    | "javascript" -> "Сначала научись правильно писать название языка!"
    | "php" -> "Ты ещё на Perl пишешь?"
    | "c++" -> "А где твой Segfault?"
    | _ -> $"Ну {l} — это ничего, но попробуй F#!"

[<EntryPoint>]
let main argv =
    printf "Какой язык программирования твой любимый? > "
    let input = System.Console.ReadLine()
    input |> generateResponse |> printfn "%s"
    0
