module UIEvents

open System
open Extensions

type Navigation = Left | Right | Tap | Swipe | Escape | Twist

type GEvent = Nav of Navigation | Service of bool

let uiEvents = Event<GEvent>()
let onUiEvent  = uiEvents.Publish

let uiCtx = Android.App.Application.SynchronizationContext

let post ev =
    async {
        do! Async.SwitchToContext uiCtx
        try
            uiEvents.Trigger ev
        with ex ->
            logE ex.Message
    }
    |> Async.Start


