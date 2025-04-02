open System

[<EntryPoint>]
let main argv =
    let generateResponse (lang:string) =
        let l = lang.ToLower()
        match l with
        | "f#" | "prolog" -> "Ты — подлиза!"
        | "python" -> "Змеиный язык? Серьёзно?"
        | "java" -> "Кофеек подождёт..."
        | "javascript" -> "Сначала научись правильно писать название языка!"
        | "php" -> "Ты ещё на Perl пишешь?"
        | "c++" -> "А где твой Segfault?"
        | _ -> $"Ну {l} — это ничего, но попробуй F#!"

    printf "Какой язык программирования твой любимый? > "
    let input = System.Console.ReadLine()
    printfn "%s" (generateResponse input)
    printf "А какой язык программирования твой второй любимый? > "
    (fun () -> System.Console.ReadLine())
    >> generateResponse
    >> printfn "%s"
    |> fun f -> f()
    0
