[<EntryPoint>]
let main argv =
    printf "r= "
    let r = System.Console.ReadLine() |> float
    printf "h= "
    let h = System.Console.ReadLine() |> float

    let solveS r =
        (r*r*3.1415)

    let solveV r h =
        let S = solveS(r)
        (S*h)

    printfn "Каррирование"
    printfn "s= %f" (solveS r)
    let Sc = solveV r
    printfn "v= %f" (Sc h)
    
    let supS = fun r -> r*r*3.1415
    
    let supV = supS >> (fun s -> fun h -> s * h)

    printfn "Суперпозиция:"
    printfn "s= %f" (supS r)
    printfn "v= %f" (supV r h)
    0
