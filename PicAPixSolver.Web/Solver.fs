module PicAPixSolver.Web.Solver

let defaultModel =
    let x, y = 30, 40
    Array2D.init x y (fun i j ->
        let r = i * 255 / (x - 1)
        let g = j * 255 / (y - 1)
        let b = 255
        sprintf "#%02x%02x%02x" r g b)