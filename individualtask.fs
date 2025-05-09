open System

let calculatePrizeFund turns =
    let finalProbs =
        (1, [|1.0|]) 
        |> Seq.unfold (fun (i, state) ->
            if i > turns then None
            else
                let nextState = 
                    [| for j in 0..i do
                        let p = 1.0 / float (i + 1)
                        let fromBlue = if j > 0 then state.[j-1] * p else 0.0
                        let fromRed = if j < state.Length then state.[j] * (1.0 - p) else 0.0
                        fromBlue + fromRed |]
                Some(nextState, (i + 1, nextState))
        )
        |> Seq.last
    
    let requiredBlue = (turns / 2) + 1
    let probability = finalProbs.[requiredBlue..] |> Array.sum
    1.0 / probability |> floor |> int

let result = calculatePrizeFund 15
printfn "The maximum prize fund is £%d" result
