// Дополнительные сведения о F# см. на http://fsharp.net
// Дополнительную справку см. в проекте "Учебник по F#".

let rec readList n = 
    if n=0 then []
    else
    let Head = System.Convert.ToInt32(System.Console.ReadLine())
    let Tail = readList (n-1)
    Head::Tail

let readData = 
    let n=System.Convert.ToInt32(System.Console.ReadLine())
    readList n

let rec writeList = function
    [] ->   let z = System.Console.ReadKey()
            0
    | (head : int)::tail -> 
                       System.Console.WriteLine(head)
                       writeList tail  

let max2 x y = if x > y then x else y

let rec accCond list (f : int -> int -> int) p acc = 
    match list with
    | [] -> acc
    | h::t ->
                let newAcc = f acc h
                if p h then accCond t f p newAcc
                else accCond t f p acc

let listMin list = 
    match list with 
    |[] -> 0
    | h::t -> accCond list (fun x y -> if x < y then x else y) (fun x -> true) h

let listMax list = 
    match list with 
    |[] -> 0
    | h::t -> accCond list max2 (fun x -> true) h

let listSum list = accCond list (fun x y -> x + y) (fun x -> true) 0

let listPr list = accCond list (fun x y -> x * y) (fun x -> true) 1

 


let f6 list = 
    match list with
    |[] -> 0
    | h::t ->   if (h % 2 = 0) then accCond t max2 (fun x -> ((x % 2) = 0)) h
                else accCond t max2 (fun x -> ((x % 2) <> 0) ) h

let rec frequency list elem count =
        match list with
        |[] -> count
        | h::t -> 
                        let count1 = count + 1
                        if h = elem then frequency t elem count1 
                        else frequency t elem count

let rec freqList list mainList curList = 
        match list with
        | [] -> curList
        | h::t -> 
                    let freqElem = frequency mainList h 0
                    let newList = curList @ [freqElem]
                    freqList t mainList newList

let pos list el = 
    let rec pos1 list el num = 
        match list with
            |[] -> 0
            |h::t ->    if (h = el) then num
                        else 
                            let num1 = num + 1
                            pos1 t el num1
    pos1 list el 1

let getIn list pos = 
    let rec getIn1 list num curNum = 
        match list with 
            |[] -> 0
            |h::t -> if num = curNum then h
                     else 
                            let newNum = curNum + 1
                            getIn1 t num newNum
    getIn1 list pos 1

let f7 list = 
    let fL = freqList list list []
    (listMax fL) |> (pos fL) |> (getIn list)           

let filter list pr = 
    let rec filter1 list pr newList = 
        match list with
        | [] -> newList
        | h::t ->
                let newnewList = newList @ [h]
                if pr h then filter1 list pr newnewList
                else filter1 list pr newList
    filter1 list pr [] 

let even n = ((n % 2) = 0)

let f8Cond list el = (even el) && (even (frequency list el 0))

let f8 list = filter list (f8Cond list)

let delEL list el = filter list (fun x -> (x <> el))

let uniq list = 
    let rec uniq1 list newList = 
        match list with
            |[] -> newList
            | h::t -> 
                        let listWithout = delEL t h
                        let newnewList = newList @ [h]
                        uniq1 listWithout newnewList
    uniq1 list [] 

let rec cifrSum n = 
    if n = 0 then 0
    else (n%10) + (cifrSum (n / 10))

let f9Cond el = ((cifrSum el) > 9) || (even el)

let f9 list = filter list f9Cond

let count x y = x + 1

let f10Cond list El = (accCond list count (fun x -> ((x * x) = El)) 0) > 0

let f10 list = accCond list count (f10Cond list) 0   

// Количество элементов после последнего максимального
// Рекурсивный подход
let countAfterLastMaxChurch list =
    let rec loop lst currentMax lastIdx idx =
        match lst with
        | [] -> (currentMax, lastIdx)
        | x :: xs ->
            if x > currentMax then
                loop xs x idx (idx + 1)
            elif x = currentMax then
                loop xs currentMax idx (idx + 1)
            else
                loop xs currentMax lastIdx (idx + 1)
    if List.isEmpty list then 0
    else
        let (maxVal, lastIndex) = loop list (List.head list) 0 0
        List.length list - lastIndex - 1

// С использованием List
let countAfterLastMaxList list =
    if List.isEmpty list then 0
    else
        let maxVal = List.max list
        let lastIndex = List.findIndexBack (fun x -> x = maxVal) list
        List.length list - lastIndex - 1

// Найти уникальный элемент
// Рекурсивный подход
let findUniqueChurch list =
    let rec findUniqueHelper lst standard =
        match lst with
        | [] -> standard
        | x :: xs ->
            if x <> standard then x
            else findUniqueHelper xs standard
    match list with
    | [] -> -1
    | [x] -> x
    | x1 :: x2 :: xs ->
        if x1 = x2 then
            findUniqueHelper xs x1
        else
            if List.exists (fun x -> x = x1) xs then x2
            else x1

// С использованием List
let findUniqueList list =
    list
    |> List.countBy id
    |> List.find (snd >> (=) 1)
    |> fst

// Элементы после первого максимального
// Рекурсивный подход
let elementsAfterFirstMaxChurch list =
    let rec findFirstMaxIndex lst currentMax currentIdx idx =
        match lst with
        | [] -> currentIdx
        | x :: xs ->
            if x > currentMax then
                findFirstMaxIndex xs x idx (idx + 1)
            else
                findFirstMaxIndex xs currentMax currentIdx (idx + 1)
    if List.isEmpty list then []
    else
        let firstMaxIdx = findFirstMaxIndex list (List.head list) 0 0
        list |> List.skip (firstMaxIdx + 1)

// С использованием List
let elementsAfterFirstMaxList list =
    if List.isEmpty list then []
    else
        let maxVal = List.max list
        let firstIdx = List.findIndex (fun x -> x = maxVal) list
        List.skip (firstIdx + 1) list

// Количество четных элементов
// Рекурсивный подход
let countEvenChurch list =
    let rec loop lst acc =
        match lst with
        | [] -> acc
        | x :: xs ->
            loop xs (if x % 2 = 0 then acc + 1 else acc)
    loop list 0

// С использованием List
let countEvenList list =
    list |> List.filter (fun x -> x % 2 = 0) |> List.length

//Среднее арифметическое модулей
// Рекурсивный подход
let averageAbsChurch list =
    let rec sumAbsLoop lst acc =
        match lst with
        | [] -> acc
        | x :: xs -> sumAbsLoop xs (acc + abs x)
    let sum = sumAbsLoop list 0
    if List.isEmpty list then 0.0 else float sum / float (List.length list)

// С использованием List
let averageAbsList list =
    if List.isEmpty list then 0.0
    else list |> List.map abs |> List.averageBy float

// Построение списков L1 и L2
// Рекурсивный подход
let l1l2Church list =
    let rec collectUnique lst acc =
        match lst with
        | [] -> acc
        | x :: xs ->
            if List.exists (fun (k, _) -> k = x) acc then
                let acc' = List.map (fun (k, c) -> if k = x then (k, c + 1) else (k, c)) acc
                collectUnique xs acc'
            else
                collectUnique xs ((x, 1) :: acc)
    let counts = collectUnique list [] |> List.rev
    let l1 = List.map fst counts
    let l2 = List.map snd counts
    (l1, l2)

// С использованием List
let l1l2List list =
    let grouped = list |> List.groupBy id
    let l1 = grouped |> List.map fst
    let l2 = grouped |> List.map (snd >> List.length)
    (l1, l2)

[<EntryPoint>]
let main argv = 
    //writeList readData
    let l = readData

    //вывод списка
    writeList l

    //функция принимающая 2 условия
    let result = accCond l (fun x y -> x+y) (fun x -> x % 2 = 0) 0
    printfn "результат %i" result

    //сумма произведение мин макс
    let sum : int = listSum l
    let pr : int= listPr l
    let min : int= listMin l
    let max : int= listMax l
    System.Console.WriteLine((sum,pr,min,max))
    
     //самый частый элемент
    let ans = f7 l
    System.Console.WriteLine(ans)
    let z = System.Console.ReadKey()

    printfn "1.1 Рекурсивный: %d" (countAfterLastMaxChurch l)
    printfn "1.1 С использованием List: %d" (countAfterLastMaxList l)

    printfn "1.11 Рекурсивный: %d" (findUniqueChurch l)
    printfn "1.11 С использованием List: %d" (findUniqueList l)

    let list1 = elementsAfterFirstMaxChurch l
    let list2 = elementsAfterFirstMaxList l
    printfn "1.21 Рекурсивный:"
    writeList list1
    printfn "1.21 С использованием List:"
    writeList list2

    printfn "1.31 Рекурсивный: %d" (countEvenChurch l)
    printfn "1.31 С использованием List: %d" (countEvenList l)

    printfn "1.41 Рекурсивный: %f" (averageAbsChurch l)
    printfn "1.41 С использованием List: %f" (averageAbsList l)

    let (list1_1, list1_2)  = l1l2Church l
    let (list2_1, list2_2) = l1l2List l
    printfn "1.51 Рекурсивный:"
    writeList list1_1
    writeList list1_2
    printfn "1.51 С использованием List:"
    writeList list2_1
    writeList list2_2
    0
