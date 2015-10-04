namespace GestureRecWear

open System
open Extensions
open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open Android.Gms.Wearable
open Android.Hardware
open Android.Support.Wearable.Views
open Android.Graphics.Drawables
open Android.Gms.Wearable
open Android.Hardware


[<Activity(Label = "Gesture Rec", MainLauncher = true)>]
type MainActivity() = 
    inherit Activity()
    let uiCtx = System.Threading.SynchronizationContext.Current
    let mutable subscription = Unchecked.defaultof<_>

    let updateService (ctx:Context) (sw:Switch) =
        async {
            try 
                let isrunning = AndroidExtensions.isServiceRunning "GestureRecWear.WearSensorService"
                do! Async.SwitchToContext uiCtx
                sw.Checked <- isrunning
                sw.Click.Add (fun ev -> 
                    if sw.Checked then
                        let i = new Intent(ctx,typeof<WearSensorService>)
                        ctx.StartService i |> ignore
                    else
                        new Intent(ctx,typeof<WearSensorService>) |> ctx.StopService |> ignore
                    )
            with ex ->
                logE ex.Message
            } |> Async.Start

    override this.OnCreate(bundle) = 
        base.OnCreate(bundle)
        this.SetContentView(Resource_Layout.Main)

        let tglService = this.FindViewById<Switch>(Resource_Id.swService)
 

        updateService this tglService
        subscription <- GlobalState.isRunning.Subscribe(fun running -> 
            logI (sprintf "global state changed: %A" running)
            tglService.Checked <- running)

    override this.OnDestroy() = 
       base.OnDestroy()
       if subscription <> Unchecked.defaultof<_> then subscription.Dispose()
       base.OnStop()


