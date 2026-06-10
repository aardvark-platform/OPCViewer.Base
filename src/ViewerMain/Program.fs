open System
open Aardvark.Base
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.UI
open Aardvark.UI.Giraffe
open Aardium

open OpcViewer.Base
open FSharp.Data.Adaptive

[<EntryPoint; STAThread>]
let main argv = 
    Aardvark.Init()
    Aardium.Init()

    ////cootrafo testing
    //CooTransformation.initCooTrafo ()
    
    //let pos = V3d(10000,1000,10000)
    //let sc = CooTransformation.getLatLonAlt pos Planet.Mars
    //Log.line "altitude: %f" sc.altitude

    //CooTransformation.deInitCooTrafo()

    use app = new OpenGlApplication()
    //let opcDir = "C:\Users\laura\VRVis\Data\CapeDesire\Surface\Cape_Desire_RGB"
    let opcDir = argv.[0];
    let axisFile = None //if argv.Length > 1 then Some(argv.[1]) else None

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

    let axisFile = argsKv |> HashMap.tryFind "axis"

    let rotate = argsList.Contains("-rotate")
    
    use instance = OpcSelectionViewer.App.app opcDir axisFile rotate |> App.start 
    //let instance = OpcOutlineTest.OutlineApp.appOutlines opcDir |> App.start 

    // use can use whatever suave server to start you mutable app. 
    // startServerLocalhost is one of the convinience functions which sets up 
    // a server without much boilerplate.
    // there is also WebPart.startServer and WebPart.runServer. 
    // look at their implementation here: https://github.com/aardvark-platform/aardvark.media/blob/master/src/Aardvark.Service/Suave.fs#L10
    // if you are unhappy with them, you can always use your own server config.
    // the localhost variant does not require to allow the port through your firewall.
    // the non localhost variant runs in 127.0.0.1 which enables remote acces (e.g. via your mobile phone)
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

    //use ctrl = new AardvarkCefBrowser()
    //ctrl.Dock <- DockStyle.Fill
    //form.Controls.Add ctrl
    //ctrl.StartUrl <- "http://localhost:4321/"
    //ctrl.ShowDevTools()
    //form.Text <- "Examples"
    //form.Icon <- Icons.aardvark 

    //Application.Run form
    0 
