//high level state machine that runs different lower-level gesture recognition sm
//high level states are the modes of the applicaition such as initial, tracking, etc.
module HSM
open FSM
open System
open Recognition

//define initial states for convenience
module Init =
    let escape      = F(Escape.start Escape.EscapeCfg.Default             , None)
    let twist       = F(Twist.start  Twist.TwistCfg.Default               , None)
    let navigation  = F(Navigation.initialQuite Navigation.NavigationCfg.Default , None)
    let sensorTest  = F(SensorTest.start                                  , None)

let rec m_start initialState event =
    match evalState initialState event with
    | F(next,Some RE_Twist) -> F (m_tracking [Init.escape; Init.navigation],Some RE_Twist)
//    | F(next,Some RE_Twist) -> F (m_tracking [Init.escape; Init.sensorTest],Some RE_Twist)
    | next                  -> F (m_start next,None)

and m_tracking states event = 
    match evalStates states event with
    | newstates,Some ev ->
        match ev with
        | RE_Escape -> F(m_start Init.twist,Some RE_Escape)
        | ev        -> F(m_tracking [Init.escape; Init.navigation],Some ev)
    | newstates, _ -> 
        F(m_tracking newstates,None)