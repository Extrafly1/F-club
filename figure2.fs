type ShapeDU =
    | Rectangle of width: float * height: float
    | Square of side: float
    | Circle of radius: float

let calculateArea shape =
    match shape with
    | Rectangle(w, h) -> w * h
    | Square(s) -> s * s
    | Circle(r) -> 3.1415 * r * r

let shapes = [
    Rectangle(4.0, 5.0)
    Square(3.0)
    Circle(2.0)
]

shapes
|> List.iter (fun s ->
    let area = calculateArea s
    printfn "Площадь фигуры: %.2f" area
)
