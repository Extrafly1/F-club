open System
open System.Text.RegularExpressions

type RussianPassport(series: string, number: string, 
                     fullName: string, birthDate: DateTime, 
                     birthPlace: string, issueDate: DateTime) as this =
    let seriesPattern = @"^\d{4}$"
    let numberPattern = @"^\d{6}$"
    
    do
        if not (Regex.IsMatch(series, seriesPattern)) then
            raise (ArgumentException("Неверный формат серии (должно быть 4 цифры)"))
        if not (Regex.IsMatch(number, numberPattern)) then
            raise (ArgumentException("Неверный формат номера (должно быть 6 цифр)"))
    
    member val Series = series
    member val Number = number
    member val FullName = fullName
    member val BirthDate = birthDate
    member val BirthPlace = birthPlace
    member val IssueDate = issueDate
    
    override this.ToString() =
        $"Паспорт РФ: {this.Series} {this.Number}\n" +
        $"Владелец: {this.FullName}\n" +
        $"Дата рождения: {this.BirthDate:dd_MM_yyyy}\n" +
        $"Место рождения: {this.BirthPlace}\n" +
        $"Дата выдачи: {this.IssueDate:dd_MM_yyyy}"
    
    interface IEquatable<RussianPassport> with
        member this.Equals(other) =
            this.Series = other.Series && 
            this.Number = other.Number
            
    override this.Equals(obj) =
        match obj with
        | :? RussianPassport as p -> (this :> IEquatable<_>).Equals(p)
        | _ -> false
        
    override this.GetHashCode() =
        hash (this.Series, this.Number)

try
    let passport1 = RussianPassport(
        "1234", "567890", 
        "Иванов Иван Иванович", 
        DateTime(1990, 5, 15),
        "г. Москва", 
        DateTime(2015, 7, 20)
    )
    
    let passport2 = RussianPassport(
        "1234", "567890", 
        "Петров Петр Петрович", 
        DateTime(1985, 3, 10),
        "г. Санкт-Петербург", 
        DateTime(2010, 4, 25)
    )
    let passport3 = RussianPassport(
        "0000", "123456", 
        "Сидоров Алексей Викторович", 
        DateTime(1975, 12, 1),
        "г. Екатеринбург", 
        DateTime(2020, 9, 5)
    )

    printfn "Паспорт 1:\n%s\n" (passport1.ToString())
    printfn "Паспорт 2:\n%s\n" (passport2.ToString())
    printfn "Паспорт 3:\n%s\n" (passport3.ToString())
    
    printfn "Паспорт1 == Паспорт2: %b" (passport1 = passport2)
    printfn "Паспорт1 == Паспорт3: %b" (passport1 = passport3)
    
    // let invalidPassport = RussianPassport("12", "123", "Тест", DateTime.Now, "Тест", DateTime.Now)
    // хотел проверить исключение, но VS кроет матом строку ещё до запуска
with
| :? ArgumentException as ex -> printfn "Ошибка: %s" ex.Message
