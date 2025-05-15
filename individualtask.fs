open System

let calculatePrizeFund turns =
    let rec loop currentTurn state =
        match currentTurn > turns with
        | true -> state
        | false ->
            let rec buildNextState j acc =
                match j > currentTurn with
                | true -> List.rev acc
                | false ->
                    let p = 1.0 / float (currentTurn + 1)
                    let fromBlue = 
                        match j > 0 with
                        | true -> List.item (j-1) state * p
                        | false -> 0.0
                    let fromRed = 
                        match j < List.length state with
                        | true -> List.item j state * (1.0 - p)
                        | false -> 0.0
                    buildNextState (j+1) (fromBlue + fromRed :: acc)
            
            let nextState = buildNextState 0 []
            loop (currentTurn + 1) nextState

    let rec sumProbabilities lst count =
        match lst, count with
        | [], _ -> 0.0
        | _, 0 -> List.sum lst
        | x::xs, _ -> sumProbabilities xs (count - 1)

    let finalProbs = loop 1 [1.0]
    let requiredBlue = turns / 2 + 1
    let probability = sumProbabilities finalProbs requiredBlue
    1.0 / probability |> floor |> int

let result = calculatePrizeFund 15
printfn "The maximum prize fund is £%d" result
