
open System
open Aardvark.Base
open Aardvark.Application.Slim
open Aardvark.UI
open Aardvark.UI.Giraffe
open Aardium

open FSharp.Data.Adaptive


[<EntryPoint>]
let main argv =

    Aardvark.Init()
    Aardium.Init()

    use app = new OpenGlApplication()

    let argsList = List.fold(fun (x:string) (y : string)-> x + " " + y) String.Empty (argv |> Array.toList)

    let argsKv = 
      argv 
        |> Array.filter(fun x -> x.Contains "=")
        |> Array.map(fun x -> 
              let kv = x.Split [|'='|]
              kv.[0],kv.[1])
        |> HashMap.ofArray

    let opcDir =
      match argsKv |> HashMap.tryFind "opc" with
      | Some dir -> dir
      | None -> failwith "need opc directory ... opc=\"[opcfilepath]\" "


    let rotate = argsList.Contains("-rotate")
    
    use instance = ViewPlanner.App.app opcDir rotate |> App.start 

    Server.startLocalhost 4321 instance.CancellationToken [ 
        MutableApp.toWebPart' app.Runtime false instance
        WebPart.ofType<Primitives.EmbeddedResources>
    ] |> ignore

    Aardium.run {
        url "http://localhost:4321/"
        width 1024
        height 768
        debug true
        log (fun msg -> Report.Line(2, $"[Aardium] {msg}"))
    }

    0 
