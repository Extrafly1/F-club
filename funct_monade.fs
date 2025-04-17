// Реализация типа Maybe
type Maybe<'a> =
    | Just of 'a
    | Nothing

// Функтор
module Functor =
    let map f maybe =
        match maybe with
        | Just x -> Just (f x)
        | Nothing -> Nothing

// Аппликативный функтор
module Applicative =
    let pure' x = Just x
    
    let apply maybeF maybeX =
        match maybeF, maybeX with
        | Just f, Just x -> Just (f x)
        | _ -> Nothing

// Монада
module Monad =
    let bind f maybe =
        match maybe with
        | Just x -> f x
        | Nothing -> Nothing
    
    let return' = Applicative.pure'

// Проверка законов функтора
let testFunctorLaws() =
    // Закон сохранения идентичности
    let law1 = Functor.map id (Just 5) = Just 5
    
    // Закон композиции
    let f x = x + 1
    let g x = x * 2
    let law2 = 
        Functor.map (f >> g) (Just 5) = (Functor.map g << Functor.map f) (Just 5)
    
    law1 && law2

// Проверка законов аппликативного функтора
let testApplicativeLaws() =
    // Закон идентичности
    let law1 = 
        Applicative.apply (Applicative.pure' id) (Just 5) = Just 5
    
    // Закон композиции
    let compose f g x = f (g x)
    let law2 = 
        Applicative.apply (
            Applicative.apply (
                Applicative.apply (
                    Applicative.pure' compose
                ) (Applicative.pure' ((+) 1))
            ) (Applicative.pure' ((*) 2))
        ) (Just 5) = Just (1 + (2 * 5))
    
    // Закон гомоморфизма
    let law3 = 
        Applicative.apply (Applicative.pure' ((*) 2)) (Applicative.pure' 3) = Applicative.pure' (2 * 3)
    
    law1 && law2 && law3

// Проверка законов монады
let testMonadLaws() =
    // Левая идентичность
    let f x = Just (x * 2)
    let law1 = 
        Monad.bind f (Monad.return' 5) = f 5
    
    // Правая идентичность
    let law2 = 
        Monad.bind Monad.return' (Just 5) = Just 5
    
    // Ассоциативность
    let g x = Just (x + 3)
    let law3 = 
        Monad.bind g (Monad.bind f (Just 5)) = Monad.bind (fun x -> Monad.bind g (f x)) (Just 5)
    
    law1 && law2 && law3

// Проверки
printfn "Законы функтора выполняются: %b" (testFunctorLaws())
printfn "Законы аппликативного функтора выполняются: %b" (testApplicativeLaws())
printfn "Законы монады выполняются: %b" (testMonadLaws())

let double x = x * 2
let maybeDouble = Applicative.pure' double

Just 5
|> Functor.map double
|> printfn "Результат map: %A" 

Applicative.apply maybeDouble (Just 3)
|> printfn "Результат apply: %A"

Monad.bind (fun x -> Just (x * 3)) (Just 4)
|> printfn "Результат bind: %A"
