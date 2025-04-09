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

let f2 list = List.nth list (List.findIndex (fun x -> x = (List.max (List.map (fun el -> List.length (List.filter (fun elem -> (elem = el)) list)) list))) (List.map (fun el -> List.length (List.filter (fun elem -> (elem = el)) list)) list))   

let f3 list = Set.toList (Set.ofList (List.filter (fun x -> (((x % 2) = 0)&&(((List.length (List.filter (fun elem -> elem = x) list)) % 2) = 0))) list))


let rec cifrSum (n : int) : int = 
    if n = 0 then 0
    else (n%10) + (cifrSum (n / 10))

let f4 (list : 'int list) = List.filter (fun x -> (((x % 2) = 0) || (((cifrSum x) % 2) = 0)))

let f5 list = List.length (List.filter (fun x -> (List.exists (fun el -> el * el = x) list)) list)

let delCount n = 
    let rec delCount n del count = 
        if del = n then count + 1
        else    let del1 = del + 1
                if (n % del) = 0 then 
                                        let count1 = count + 1
                                        delCount n del1 count1
                else
                                        delCount n del1 count
    delCount n 1 0

let f6 list1 list2 list3 = List.zip3 (List.rev (List.sort list1)) (List.sortBy (fun x -> (cifrSum x)) list2) (List.rev (List.sortBy (fun x -> (delCount x)) list3))

type 'string btree = 
    Node of 'string * 'string btree * 'string btree
    | Nil

let prefix root left right = (root(); left(); right())
let infix root left right = (left(); root(); right())
let postfix root left right = (left(); right(); root())

[<EntryPoint>]
let main argv = 
    //Самый частый элемент
    let list7 = [1; 2; 2; 3; 3; 3; 4]
    printfn "%A -> %d" list7 (f2 list7)

    //Элементы-квадраты
    let list8 = [16; 2; 4; 3; 9; 5]
    printfn "%A -> %d" list8 (f5 list8)

    //Кортежи из трех списков
    let list9_1 = [5; 3; 7]
    let list9_2 = [123; 45; 9]
    let list9_3 = [12; 7; 24]
    printfn "%A" (f6 list9_1 list9_2 list9_3)

    printf "Введите элементы списка (сначала размер, затем элементы): "
    let l = readData
    printfn "%d" (f2 l)
    writeList (f3 l) |> ignore
    0
