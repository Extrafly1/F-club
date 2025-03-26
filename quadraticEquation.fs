type 'a option = Some of 'a | None

type SolveResult =
    | None
    | Linear of float
    | Quadratic of float*float

let solve a b c =
    let D = b*b-4.0*a*c
    if a=0.0 then
        if b = 0.0 then None
        else Linear(-c/b)
    else
        if D<0 then None
        else Quadratic(((-b+sqrt(D))/(2.0*a),(-b-sqrt(D))/(2.0*a)))

let res = solve 1.0 2.0 -3.0

match res with
    | None -> printf "None"
    | Linear(x) -> printf "%f" x
    | Quadratic(x1, x2) when x1=x2 -> printf "%f" x1
    | Quadratic(x1, x2) -> printf "%f, %f" x1 x2