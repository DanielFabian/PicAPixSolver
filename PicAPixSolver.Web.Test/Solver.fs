module PicAPixSolver.Web.Test.Solver

open PicAPixSolver.Web.Solver
open FsUnit
open NUnit.Framework

let b = 0uy, 0uy, 0uy
let r = 255uy, 0uy, 0uy
let g = 0uy, 255uy, 0uy
let w = white

let toCol = function
    | 0uy, 0uy, 0uy -> "b"
    | 255uy, 0uy, 0uy -> "r"
    | 0uy, 255uy, 0uy -> "g"
    | 255uy, 255uy, 255uy -> "w"
    | color -> sprintf "%A" color


let candidatesMatch l n exp =

    let cands = candidates l n
    if cands <> exp then
        printfn "candidates:"
        let print =
            List.iter (
                List.map toCol
                >> String.concat " "
                >> printfn "%s")

        print cands

        printfn "expected:"

        print exp
        Assert.Fail()

let [<Test>] ``candidates 3 [3, b]`` () =
    candidatesMatch 3 [3, b] [[b; b; b]]

let [<Test>] ``candidates 2 [1, b]`` () =
    candidatesMatch 2 [1, b]
        [[b; w]
         [w; b]]

let [<Test>] ``candidates 3 [1, b]`` () =
    candidatesMatch 3 [1, b]
        [[b; w; w]
         [w; b; w]
         [w; w; b]]

let [<Test>] ``candidates 3 [1, b; 1, b]`` () =
    candidatesMatch 3 [1, b; 1, b]
        [[b; w; b]]

let [<Test>] ``candidates 4 [1, b; 1, b]`` () =
    candidatesMatch 4 [1, b; 1, b]
        [[b; w; b; w]
         [b; w; w; b]
         [w; b; w; b]]

let [<Test>] ``candidates 5 [1, b; 1, r; 1, g]`` () =
    candidatesMatch 5 [1, b; 1, r; 1, g]
        [[b; r; g; w; w]
         [b; r; w; g; w]
         [b; r; w; w; g]
         [b; w; r; g; w]
         [b; w; r; w; g]
         [b; w; w; r; g]
         [w; b; r; g; w]
         [w; b; r; w; g]
         [w; b; w; r; g]
         [w; w; b; r; g]]
         
let [<Test>] ``candidates 5 [1, b; 1, r; 1, r]`` () =
    candidatesMatch 5 [1, b; 1, r; 1, r]
        [[b; r; w; r; w]
         [b; r; w; w; r]
         [b; w; r; w; r]
         [w; b; r; w; r]]

let constraintsMatch l n exp =
    let constraints = candidates l n |> constraintSets
    let exp = List.map Set.ofList exp
    if constraints <> exp then
        printfn "constraints:"
        let print =
            List.map (
                Set.map toCol
                >> String.concat ", "
                >> sprintf "{%s}")
            >> String.concat "; " 
            >> printfn "[%s]"

        print constraints

        printfn "expected:"

        print exp

        Assert.Fail()

let [<Test>] ``constraintSets 3 [2, b]`` () =
    constraintsMatch 3 [2, b] 
        [[b;w]; [b]; [b;w]]

let [<Test>] ``constraintSets 3 [1, b; 1, b]`` () =
    constraintsMatch 3 [1, b; 1, b] 
        [[b]; [w]; [b]]

let [<Test>] ``problem 1 should terminate`` () =
    solve problem1 |> ignore

let [<Test>] ``problem 2 should terminate`` () =
    solve problem2 |> ignore

let [<Test>] ``problem 3 should terminate`` () =
    solve problem3 |> ignore

let [<Test>] ``problem 4 should terminate`` () =
    solve problem4 |> ignore

