namespace PicAPixSolver.Web.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Web
open System.Web.Mvc
open System.Web.Mvc.Ajax
open PicAPixSolver.Web.Solver

type HomeController() =
    inherit Controller()
    member this.Index () = this.View()
    member this.Board () = this.View(defaultModel)
