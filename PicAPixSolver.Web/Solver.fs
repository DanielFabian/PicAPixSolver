module PicAPixSolver.Web.Solver

type Color = byte * byte * byte
type Number = int * Color

type Problem =
    { Vertical : Number list list
      Horizontal : Number list list }

let asBlack numbers =
    let black = 0uy, 0uy, 0uy
    
    numbers
    |> List.map (List.map (fun n -> n, black)) 

let white = 255uy, 255uy, 255uy

let problem1 =
    let vertical =
        [
            [ 3 ]
            [ 1; 1 ]
            [ 2; 2 ]
            [ 1; 1 ]
            [ 3 ]
        ]

    let horizontal =
        [
            [ 3 ]
            [ 1; 1; 1 ]
            [ 1; 1 ]
            [ 1; 1; 1 ]
            [ 3 ]
        ]

    { Vertical = asBlack vertical
      Horizontal = asBlack horizontal }

let problem2 =
    let vertical =
        [
            [ 3 ]
            [ 2; 2 ]
            [ 1; 1; 1]
            [ 2; 2 ]
            [ 3 ]
            [ 1 ]
            [ 1; 1 ]
            [ 1; 2 ]
            [ 2 ]
            [ 1 ]
        ]

    let horizontal =
        [
            [ 3; 1 ]
            [ 2; 2; 1 ]
            [ 1; 1; 6 ]
            [ 2; 2; 1 ]
            [ 3; 1 ]
        ]

    { Vertical = asBlack vertical
      Horizontal = asBlack horizontal }

let problem3 =
    let g = 0uy, 255uy, 0uy
    let r = 255uy, 0uy, 0uy
    let b = 0uy, 0uy, 0uy
    let br = 127uy, 82uy, 23uy
    let sk = 249uy, 150uy, 107uy
    let vertical =
        [
            [ 2, g; 2, r; 2, g; 5, g; 2, r; 1, g ]
            [ 2, g; 2, r; 2, g; 2, r; 2, g; 2, r; 1, g ]
            [ 2, g; 2, r; 4, g ]
            [ 2, g; 4, b; 2, r; 2, g ]
            [ 1, g; 4, b; 1, sk; 2, r; 1, br; 1, br ]
            [ 3, b; 2, sk; 2, sk; 3, br ]
            [ 1, b; 3, sk; 1, sk; 2, br ]
            [ 2, sk; 2, sk; 3, br ]
            [ 1, sk; 3, b; 1, sk; 2, b; 2, sk; 2, br; ]
            [ 2, sk; 6, b; 1, sk; 1, g; 2, br ]
            [ 2, sk; 4, b; 2, g; 2, br ]
            [ 1, sk; 3, b; 2, g; 1, br ]
            [ 1, sk; 4, r; 2, br; 1, br ]
            [ 2, sk; 4, r; 1, r; 3, br ]
            [ 1, sk; 4, r; 2, r; 2, br ]
            [ 3, r; 2, r; 3, r; 2, br ]
            [ 2, r; 2, r; 2, r; 1, r; 2, br ]
            [ 2, r; 2, r; 3, r; 2, br ]
            [ 3, r; 2, r; 1, r; 2, br ]
            [ 8, r; 2, br ]
        ]

    let horizontal =
        [
            [ 2, g; 2, g; 5, sk ]
            [ 4, g; 3, sk; 2, sk; 3, r ]
            [ 2, r; 1, g; 2, b; 2, b; 5, r ]
            [ 2, r; 4, b; 3, b; 3, r; 2, r ]
            [ 1, g; 3, b; 2, sk; 4, b; 6, r; 1, r ]
            [ 2, g; 2, b; 4, sk; 3, b; 3, r; 4, r ]
            [ 1, g; 1, b; 3, sk; 4, b; 4, r; 2, r ]
            [ 1, g; 2, b; 1, r; 2, r; 1, r ]
            [ 1, g; 2, r; 3, sk; 2, r; 1, r; 1, r ]
            [ 1, g; 2, r; 4, sk; 2, r; 1, r; 1, r ]
            [ 2, g; 2, r; 1, sk; 2, g; 1, r; 2, r ]
            [ 3, g; 2, r; 3, g; 1, br; 3, r ]
            [ 2, r; 2, g; 4, br; 2, br ]
            [ 2, r; 2, g; 6, br; 7, br ]
            [ 3, g; 2, br; 13, br ]
        ]

    { Vertical = vertical
      Horizontal = horizontal }

let candidates length numbers =
    let rec candidates = function
        | 0, [], _ -> [[]]
        | len, [], _ when len > 0 -> [List.replicate len white]
        | len, [], _ -> []
        | l, (num, col)::rest, _ when num > l -> []
        | l, ((num, col)::rest as numbers), oldCol ->
            [
                if Some col = oldCol then
                    yield! candidates (l - (num + 1), rest, Some col) |> List.map (fun l -> white :: List.replicate num col @ l)
                else
                    yield! candidates (l - num, rest, Some col) |> List.map (fun l -> List.replicate num col @ l)
                yield! candidates (l - 1, numbers, oldCol) |> List.map (fun l -> white :: l)
            ]

    candidates (length, numbers, None)

let constraintSets candidates =
    let len = List.head candidates |> List.length
    candidates
    |> List.fold (List.map2 (fun possibleColors fieldColor -> Set.add fieldColor possibleColors))
        (List.replicate len Set.empty)    

type SolverBoard = Color Set [,]


let problemInfo problem =
    let colors =
        problem.Vertical @ problem.Horizontal
        |> List.map (List.map snd >> set)
        |> Set.unionMany
        |> Set.add white
    let height, width = problem.Vertical.Length, problem.Horizontal.Length

    colors, height, width

let rec reduce problem board =
    let newBoard = Array2D.copy board
    let height, width = Array2D.length1 board, Array2D.length2 board
    problem.Vertical
    |> List.iteri (fun row line ->
        candidates width line
        |> List.filter (List.mapi (fun col value -> Set.contains value newBoard.[row, col]) >> List.forall id)
        |> constraintSets
        |> List.iteri (fun col colors -> newBoard.[row, col] <- Set.intersect colors newBoard.[row, col]))

    problem.Horizontal
    |> List.iteri (fun col line ->
        candidates height line
        |> List.filter (List.mapi (fun row value -> Set.contains value newBoard.[row, col]) >> List.forall id)
        |> constraintSets
        |> List.iteri (fun row colors -> newBoard.[row, col] <- Set.intersect colors newBoard.[row, col]))

    if newBoard = board then
        for i = 0 to width - 1 do
            for j = 0 to width - 1 do
                if Set.count newBoard.[i, j] <> 1 then 
                    printfn "%A" board
                    failwithf "Not solved board"
        newBoard |> Array2D.map (Seq.exactlyOne)
    else reduce problem newBoard

let solve problem =
    let colors, height, width = problemInfo problem
    let matrix = Array2D.create height width colors
    
    reduce problem matrix

let defaultModel =
//    let x, y = 30, 40
//    Array2D.init x y (fun i j ->
//        let r = i * 255 / (x - 1)
//        let g = j * 255 / (y - 1)
//        let b = 255
//        sprintf "#%02x%02x%02x" r g b)
    solve problem3
    |> Array2D.map (fun (r, g, b) -> sprintf "#%02x%02x%02x" r g b)
