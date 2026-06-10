(*

Thomas Ortners Drawing Example

*)
open System

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.UI
open Aardvark.UI.Giraffe

open Aardium

[<EntryPoint; STAThread>]
let main argv = 
    Aardvark.Init()
    Aardium.Init()

    // media apps require a runtime, which serves as renderer for your render controls.
    // you can use OpenGL or VulkanApplication.
    let useVulkan = false

    let runtime, disposable =
        if useVulkan then
            let app = new Aardvark.Rendering.Vulkan.HeadlessVulkanApplication()
            app.Runtime :> IRuntime, app :> IDisposable
        else
            let app = new OpenGlApplication()
            app.Runtime :> IRuntime, app :> IDisposable
    use __ = disposable
    
    let app = ExampleApp.app

    use instance = 
        app |> App.start

    Server.startLocalhost 4321 instance.CancellationToken [ 
        MutableApp.toWebPart' runtime false instance
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