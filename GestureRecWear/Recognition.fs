//common types and functions for gesture recognition
module Recognition
open FSM

type RecognizedEvents =
    | RE_Twist
    | RE_Escape
    | RE_Swipe
    | RE_Tap
    | RE_Left
    | RE_Right

[<Literal>]
let Accelerometer       = 1
[<Literal>]
let Gravity             = 9
[<Literal>]
let Gyroscope           = 4
[<Literal>]
let LinearAcceleration  = 10
[<Literal>]
let RotationVector      = 11

type SnsrEvent = {Snsr:int; Ticks:int64; X:float32; Y:float32; Z:float32}
    with static member Default = {Snsr=0;Ticks=0L;X=0.f;Y=0.f;Z=0.f}

let updateAvg (prevAvg,count) newVal = 
    let count' = (count + 1.f)
    let newAvg = (prevAvg*count + newVal) / count'
    (newAvg,count')


//code below is used for debugging state machines only
//it is used to output state machine data to the console
//here you can select the state machines for which data
//is printed out (instead of modifying the sm code)

type SMType = 
    | Twist | Escape | LR | PreTap | Tap  | Nav | PreSwipe | Swipe
    | ST | ST_LA | ST_GR | ST_GY
    with 
        static member out sm a = //set to true for which you want to see ouput
            match sm with
            | Escape    -> false
            | Twist     -> false
            | LR        -> false
            | PreTap    -> false
            | Tap       -> false
            | PreSwipe  -> false
            | Swipe     -> false
            | Nav       -> false
            | ST        -> false
            | ST_GR     -> false
            | ST_GY     -> false
            | ST_LA     -> false
            |> function true -> printfn "%A - %A" sm a | _ -> ()// printfn "."
        static member (%) (y:SMType,a:obj) = SMType.out y a