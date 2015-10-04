namespace GestureRec

open System
open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open Android.Hardware
open Extensions
open UIEvents
open Android.Support.V4.App

[<Activity(Label = "Gesture Rec", MainLauncher = true)>]
type MainActivity() = 
    inherit FragmentActivity ()
    let uiCtx = System.Threading.SynchronizationContext.Current
    let mutable eventSubscription = Unchecked.defaultof<_>

    let withProtectionDo comp uiUpdate =
        async {
            try
                do! comp
            with ex ->
                logE ex.Message
            do! Async.SwitchToContext uiCtx
            do uiUpdate()
        } |> Async.Start


    override this.OnCreate(bundle) = 
        base.OnCreate(bundle)
        this.SetContentView(Resource_Layout.Main)
        // 
        let swStart     = this.FindViewById<Switch>(Resource_Id.swStart)
        let txEvent     = this.FindViewById<TextView>(Resource_Id.txEvent)

        eventSubscription <- onUiEvent.Subscribe( function 
            |Service t -> swStart.Checked <- t 
            |Nav n ->
                let t = sprintf "%A" n
                txEvent.Text <- t
            | _ -> ())

        swStart.Click.Add(fun ev ->
            if swStart.Checked then
                new Intent(this,typeof<HostSensorService>)
                |> this.StartService 
                |> ignore
            else
                new Intent(this,typeof<HostSensorService>)
                |> this.StopService 
                |> ignore
            )
        //
        let fc = this.FragmentManager.FindFragmentById(Resource_Id.fragmentContainer)
        let tx = this.FragmentManager.BeginTransaction()
        let nf = new NavigationFragment()
        tx.Add(Resource_Id.fragmentContainer,nf) |> ignore
        tx.Commit() |> ignore

    override this.OnDestroy() =
        base.OnDestroy()
        if eventSubscription <> Unchecked.defaultof<_> then eventSubscription.Dispose()
 