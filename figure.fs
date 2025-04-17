open System

[<AbstractClass>]
type Shape() =
    abstract member Area: float

type IPrint =
    abstract member Print: unit -> unit

type Rectangle(width: float, height: float) =
    inherit Shape()
    member this.Width = width
    member this.Height = height
    override this.Area = this.Width * this.Height
    override this.ToString() =
        sprintf "Прямоугольник: ширина = %.2f, высота = %.2f, площадь = %.2f" this.Width this.Height this.Area
    interface IPrint with
        member this.Print() = Console.WriteLine(this.ToString())

type Square(side: float) =
    inherit Rectangle(side, side)
    override this.ToString() =
        sprintf "Квадрат: сторона = %.2f, площадь = %.2f" side this.Area

type Circle(radius: float) =
    inherit Shape()
    member this.Radius = radius
    override this.Area = Math.PI * radius * radius
    override this.ToString() =
        sprintf "Круг: радиус = %.2f, площадь = %.2f" radius this.Area
    interface IPrint with
        member this.Print() = Console.WriteLine(this.ToString())

let rect = Rectangle(4.0, 5.0)
let square = Square(3.0)
let circle = Circle(2.0)

(rect :> IPrint).Print()
(square :> IPrint).Print()
(circle :> IPrint).Print()
