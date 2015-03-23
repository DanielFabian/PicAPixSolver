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

let problem4 =
    let vertical =
        [
            [                                                                             64;  7; 27 ]
            [                                                                              1;  9;  1 ]
            [                                                                             63; 10; 25 ]
            [                                                  1;  1;  1;  1;  1;  3;  7;  1;  1;  1 ]
            [                                          1;  1;  1;  8;  1;  1;  2;  2;  5;  1;  1;  1 ]
                                                          
            [                              1;  1;  1;  7;  3;  1;  1;  2;  2;  5;  2;  4;  1;  1;  1 ]
            [                                                 18;  6; 18;  2;  2;  3;  1;  1;  3; 20 ]
            [                              1;  1;  1;  7;  3;  1;  4;  2;  3;  1;  1;  3;  1;  1;  1 ]
            [                              1;  1;  1; 10;  4;  1;  1;  1;  4;  3;  1;  4;  1;  1;  1 ]
            [                          1;  1;  1;  3;  3;  4;  1;  1;  4;  4;  3;  3;  4;  1;  1;  1 ]
                                                          
            [                                         17;  2;  2;  4;  8;  1;  1;  2;  4;  3;  4; 23 ]
            [                      1;  1;  1;  2;  3;  1;  6;  3;  1;  1;  2;  4;  4;  1;  6;  9;  1 ]
            [                      1;  1;  2;  1;  1;  2;  3;  2;  1;  1;  2;  4;  4;  4;  6;  9;  1 ]
            [                          1;  3;  1;  1;  1;  2;  3;  1; 15;  2;  4;  2;  4;  1;  9;  1 ]
            [                              3;  1;  1;  4;  2;  2;  2;  3;  1;  2;  2;  1;  4;  6; 16 ]
                                                          
            [                                  1;  8;  1;  1;  1;  3;  3;  1; 13;  2;  4;  5; 11;  1 ]
            [                          1;  9;  1;  2;  3;  8;  2;  1;  1;  2;  1;  4;  1;  4; 13;  1 ]
            [                                      1; 10;  5;  7;  3;  1;  1; 10;  4;  1;  2; 13;  1 ]
            [                          1;  1;  1; 11;  5;  3;  3;  4;  1;  2;  1;  1;  4;  1;  1; 20 ]
            [                              6;  1;  1; 10;  1;  3;  2;  8;  5;  2;  4;  4;  3; 15;  1 ]
                                                          
            [                              1;  1;  1;  1; 12;  2;  3;  2;  8;  7;  2;  1;  4;  2; 16 ]
            [                  1;  1;  1;  1;  1;  9;  4;  3;  2;  9;  3;  3;  2;  2;  4;  1; 17;  2 ]
            [                  1;  1;  1;  1; 10;  2;  1;  3;  9;  3;  3;  4;  5;  3;  4;  5;  1;  1 ]
            [              6;  1;  1; 10;  3;  2;  1;  2;  1;  8; 13;  1;  6;  1;  7;  2;  1;  2;  2 ]
            [                      1; 13;  1;  1;  1;  1;  1;  1;  7;  3;  3; 12;  9;  3;  1;  1;  1 ]
                                                          
            [                  1;  1;  9;  1;  3;  2;  1;  1;  1;  6;  3;  3; 10; 11;  1;  4;  2;  1 ]
            [                  1;  4;  8;  1;  2;  3;  2;  1;  1;  5; 19; 10;  1;  5;  4;  1;  1;  4 ]
            [      1;  1;  1;  2;  7;  1;  2;  1;  2;  1;  1;  1;  4;  3; 13;  2;  2;  4;  1;  1;  1 ]
            [  1;  1;  1;  3;  6;  1;  1;  3;  1;  1;  2;  1;  3;  3; 12;  1;  1;  1;  2;  1;  1;  1 ]
            [      5;  6;  1;  1;  1;  1;  1;  1;  1;  1;  2;  3;  5;  3;  1;  1;  1;  2;  2;  1;  1 ]
                                                          
            [              8;  7;  1;  1;  1;  1;  1;  1;  1;  2; 18;  6;  6;  2;  1;  2;  3;  1;  3 ]
            [     10;  4;  2;  1;  1;  1;  1;  1;  1;  1;  1;  3;  2;  1;  4;  1;  3;  2;  4;  1;  1 ]
            [  3;  1;  6;  2;  3;  1;  1;  1;  1;  1;  1;  3;  1; 11;  2;  2;  7;  2;  5;  1;  2;  1 ]
            [          1;  8;  2;  2;  1;  1;  1;  1;  1;  1;  3; 12;  1;  1;  7;  4;  3;  3;  3;  1 ]
            [                  1;  1;  7;  1;  3;  1;  1;  1;  1;  1; 10; 10;  1;  7;  1;  1;  3; 10 ]
                                                          
            [  1;  1; 11;  1;  3;  1;  1;  1;  1;  1;  3;  1; 10;  1;  5;  1;  3;  1;  1;  5;  1;  1 ]
            [  8;  6;  2;  1;  1;  1;  1;  1;  3;  1;  2;  9;  1;  2;  1;  2;  3;  1;  1;  1;  1;  1 ]
            [  2; 10;  2;  2;  2;  1;  1;  1;  3;  1; 10;  2;  1;  2;  1;  2;  3;  1;  1;  1;  1;  1 ]
            [              5;  6;  4;  4;  1;  1;  1;  8; 11;  1;  7;  1;  2;  1;  1;  1;  1;  1; 11 ]
            [     11;  4;  1;  1;  1;  1; 15;  1;  2;  1;  1;  2;  2;  1;  1;  1;  1;  1;  1;  1;  1 ]
                                                          
            [                     12;  9;  1;  1; 15;  5;  1;  5;  1;  1;  1;  1;  1;  1;  1;  1;  1 ]
            [                      2; 10;  1;  6;  1; 15;  4;  3;  3;  1;  1;  1;  1;  1;  1;  1;  1 ]
            [                                         13;  1;  1;  7; 15;  5;  4;  1;  1;  1;  1; 11 ]
            [                                     13;  4;  1;  1;  1; 15;  2;  9;  1;  1;  1;  1;  1 ]
            [                                 13;  7;  1;  1;  1; 15;  1;  3;  1;  3;  1;  1;  5;  1 ]
                                                          
            [                                      1;  3;  3;  2;  6;  1;  1; 14;  3;  3;  3;  8;  1 ]
            [                                                      1;  1;  2;  6;  8; 14;  3; 10;  1 ]
            [                                                      1;  1;  2;  4;  3;  1; 14;  2; 14 ]
            [                              1;  3;  3;  2;  4;  4;  1;  1; 14;  1;  1;  2;  1;  9;  1 ]
            [                              1;  3;  3;  3;  3;  5;  1; 14;  1;  2;  2;  1;  2;  4;  1 ]
                                                          
            [                              1;  3;  3;  3;  3; 12; 14;  2;  1;  3;  1;  1;  1;  4;  1 ]
            [                          1;  3;  3;  3;  2;  8;  1; 15;  1;  1;  1;  3;  4;  1;  4;  3 ]
            [                                      1;  3;  3; 10;  1;  1;  1; 15;  1;  2;  3;  4; 12 ]
            [                                  1;  3;  3;  5;  4;  1;  1; 10;  3;  6;  1;  5;  6;  1 ]
            [                                      1;  3;  3;  4;  3;  7;  7;  6;  2;  5;  1;  2;  2 ]
                                                          
            [                          1;  3;  2;  4;  4;  1;  1;  5;  4; 14;  1;  1;  1;  1;  2;  2 ]
            [                                          1;  3; 16;  1;  1;  3;  1;  2;  6;  2;  5;  5 ]
            [                                          1;  3;  4;  1;  1;  1;  2;  1;  5;  1;  4;  8 ]
            [                                                              2;  8;  9;  3;  2;  2; 11 ]
            [                                                                              8;  4;  6 ]
        ]

    let horizontal =
        [
            [1]
            [1]
        ]

    { Vertical = asBlack vertical
      Horizontal = asBlack horizontal }

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
    
    let colorSum =
        List.concat
        >> Seq.groupBy snd
        >> Seq.map (fun (col, nums) -> col, Seq.sumBy fst nums)
        >> Map.ofSeq

    let vCols, hCols = colorSum problem.Vertical, colorSum problem.Horizontal
    if vCols <> hCols then
        failwithf "malformed problem, the color sum does not match. Vertical %A; Horizontal: %A" vCols hCols

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
