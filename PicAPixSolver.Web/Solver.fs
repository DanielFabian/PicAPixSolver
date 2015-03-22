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

let buildDlxMatrix problem = 
    let colors =
        problem.Vertical @ problem.Horizontal
        |> List.map (List.map snd >> set)
        |> Set.unionMany
        |> Set.add white
        |> Set.toList
        |> List.rev
        |> List.mapi (fun i c -> c, i)
        |> Map.ofList

    let height, width = problem.Vertical.Length, problem.Horizontal.Length

    let dlxMatrix = 
        Array.init height (fun _ ->
            Array.init width (fun _ ->
                Array.init colors.Count (fun _ ->
                    [| Array.zeroCreate (height * width); Array.zeroCreate height; Array.zeroCreate width |])))

    let nColors = colors.Count


    let inline idx x y =
        x * width + y

    for i = 0 to height - 1 do
        for j = 0 to width - 1 do
            for k = 0 to nColors - 1 do
                dlxMatrix.[i].[j].[k].[0].[idx i j] <- 1

    problem.Vertical
    |> List.iteri (fun row line -> 
        candidates width line
        |> constraintSets
        |> List.iteri (fun col ->
            Set.iter (fun color -> dlxMatrix.[row].[col].[colors.[color]].[1].[row] <- 1)))


    problem.Horizontal
    |> List.iteri (fun col line -> 
        candidates width line
        |> constraintSets
        |> List.iteri (fun row ->
            Set.iter (fun color -> dlxMatrix.[row].[col].[colors.[color]].[2].[col] <- 1)))

    dlxMatrix

let dlxToArray2D dlxMatrix =
    let height = Array.length dlxMatrix
    let width = Array.length dlxMatrix.[0]
    let nColors = Array.length dlxMatrix.[0].[0]

    Array2D.init (height * width * nColors) (height * width + height + width) (fun i j ->
        let color = i % nColors
        let col = i / nColors % width
        let row = i / nColors / width
        match j with
        | pos when pos < height * width -> (dlxMatrix.[row].[col].[color] : _ [][]).[0].[pos]
        | v when v < height * width + height -> dlxMatrix.[row].[col].[color].[1].[v - height * width]
        | h -> dlxMatrix.[row].[col].[color].[2].[h - height * width - height])
    
let defaultModel =
//    let x, y = 30, 40
//    Array2D.init x y (fun i j ->
//        let r = i * 255 / (x - 1)
//        let g = j * 255 / (y - 1)
//        let b = 255
//        sprintf "#%02x%02x%02x" r g b)
    buildDlxMatrix problem1
    |> dlxToArray2D
    |> Array2D.map (
        function
        | 0 -> "#ffffff"
        | _ -> "#000000")