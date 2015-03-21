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

let defaultModel =
    let x, y = 30, 40
    Array2D.init x y (fun i j ->
        let r = i * 255 / (x - 1)
        let g = j * 255 / (y - 1)
        let b = 255
        sprintf "#%02x%02x%02x" r g b)