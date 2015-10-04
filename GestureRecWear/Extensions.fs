﻿module Extensions
open System
let tag = "gesture.rec"
let logI d = 
    Android.Util.Log.Info(tag,d)  |> ignore
let logE d = Android.Util.Log.Error(tag,d) |> ignore

let lowercase (s:string) = s.ToLower()
let split cs (s:string) = s.Split(cs |> Seq.toArray)
let join sep (xs:string array) = System.String.Join(sep,xs)
