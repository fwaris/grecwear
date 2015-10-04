namespace GestureRec

open Extensions
open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.IO
open AndroidExtensions
open UIEvents

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open Android.Gms.Wearable
open Android.Hardware
open Android.Gms.Common.Apis
open Android.Gms.Common.Data

[<Service>] 
[<IntentFilter([|WearableListenerService.BindListenerIntentAction|])>]
type HostSensorService() = 
    inherit WearableListenerService()

    let sendMsg path msg =
        async {
            try
            let! gapi = GmsExtensions.connect()
            do! GmsExtensions.sendWearMessage path msg
            with ex ->
                logE (sprintf "sendMsg %s" ex.Message)
         }
         |> Async.Start

    let toEvent = function
        | "Twist"  -> Twist
        | "Left"   -> Left
        | "Right"  -> Right
        | "Tap"    -> Tap
        | "Swipe"  -> Swipe
        | "Escape" -> Escape
        | x -> failwithf "unrecognized event %s" x

    override x.OnMessageReceived(msg) =
            let d = msg.GetData()
            let p = msg.Path
            let s = msg.SourceNodeId
            match p with
            | Constants.p_event -> d |> Encoding.UTF8.GetString |> toEvent |> Nav |> post
            | p -> 
                logE (sprintf "invalid path %s" p)
           
    override x.OnDataChanged(dataEvents) =
        logI "onDataChanged"
        try
            let events = FreezableUtils.FreezeIterable (dataEvents)
            dataEvents.Close()
            //DataReceiver.processDataEvents events
        with ex ->
           logE ex.Message

    override x.OnStart(i,id) =
        base.OnStart(i,id)
        UIEvents.post (UIEvents.Service true)
        logI "host service started"
        sendMsg Constants.p_start [||]

    override x.OnDestroy() =
        base.OnDestroy()
        UIEvents.post (UIEvents.Service false)
        logI "host service stopped"
        sendMsg Constants.p_stop [||]