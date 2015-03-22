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

let defaultModel =
    let x, y = 30, 40
    Array2D.init x y (fun i j ->
        let r = i * 255 / (x - 1)
        let g = j * 255 / (y - 1)
        let b = 255
        sprintf "#%02x%02x%02x" r g b)