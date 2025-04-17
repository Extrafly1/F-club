open System

type AgentMessage =
    | Print of string
    | Calculate of int * int
    | GetCount of AsyncReplyChannel<int>
    | Stop

let agent = MailboxProcessor.Start(fun inbox ->
    let rec loop count = async {
        let! msg = inbox.Receive()
        
        match msg with
        | Print s -> 
            printfn $"Сообщение: {s}"
            return! loop (count + 1)
            
        | Calculate (a, b) ->
            printfn $"Сумма {a} и {b} = {a + b}"
            return! loop (count + 1)
            
        | GetCount replyChannel ->
            replyChannel.Reply(count)
            return! loop count
            
        | Stop ->
            printfn "Агент остановлен"
            return ()
    }
    loop 0
)

agent.Post(Print "Hello World")
agent.Post(Calculate(10, 5))

async {
    let! count = agent.PostAndAsyncReply(GetCount)
    printfn $"Обработано сообщений: {count}"
} |> Async.RunSynchronously

agent.Post(Stop)

let delayedAgent = MailboxProcessor.Start(fun inbox ->
    let rec loop () = async {
        let! msg = inbox.Receive()
        match msg with
        | Print s ->
            do! Async.Sleep 2000
            printfn $"[Задержанное] {s}"
            return! loop()
        | _ -> return! loop()
    }
    loop()
)

delayedAgent.Post(Print "Сообщение с задержкой 2 сек")
printfn "Это сообщение появится сразу"
