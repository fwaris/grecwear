//recognize escape gesture
module Escape
open Recognition
open FSM

type EscapeCfg = 
    {
        mutable x_gravity_thrsld    : float32
        mutable time_thrsld         : int64
    }
    with
    static member Default = {
        x_gravity_thrsld    = -0.7f
        time_thrsld         = 1000000000L
    }

let rec start cfg = function
    | {Snsr=Gravity;Ticks=t;X=x;Y=y;Z=z} when 
        x < cfg.x_gravity_thrsld
        -> 
        Escape % ("start->wait",cfg.x_gravity_thrsld,x)
        F (wait cfg t,None)
    | _ 
        ->
        F(start cfg, None)

and wait cfg t_start = function
    | {Snsr=Gravity;Ticks=t;X=x;Y=y;Z=z} when 
        x < cfg.x_gravity_thrsld
        && t - t_start > cfg.time_thrsld
        -> 
        Escape % ("RE_Escape",t-t_start,x,y,z)
        F (start cfg, Some RE_Escape)
    | {Snsr=Gravity;Ticks=t;X=x;Y=y;Z=z} when 
        x < cfg.x_gravity_thrsld
        && t - t_start < cfg.time_thrsld
        -> 
        Escape % ("wait->wait",t-t_start,x,y,z)
        F (wait cfg t_start,None)
    | {Snsr=Gravity;Ticks=t;X=x;Y=y;Z=z} 
        -> 
        Escape % ("wait->start")
        F (start cfg ,None)
    | _
        -> 
        F (wait cfg t_start ,None)


(* twist based escapse gesture

type EscapeCfg =
    {
        mutable quiet_threshold   : float32
        mutable yzstart_threshold : float32
        mutable xstart_threshold  : float32
        mutable ts_quite          : int64
        mutable ts_min            : int64
        mutable twistcfg          : Twist.TwistCfg
    }
    with
    static member Default = {
        quiet_threshold   = 1.0f
        yzstart_threshold = 0.8f
        xstart_threshold  = -0.1f
        ts_quite          = 50000000L
        ts_min            = 100000000L
        twistcfg          = {
                                x_threshold  = 3.0f
                                yz_threshold = 1.2f
                                ts_threshold = 400000000L 
                            }
        }


let rec start cfg = function 
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when 
        abs(x) <= cfg.quiet_threshold
        && abs(y) <= cfg.quiet_threshold
        && abs(z) <= cfg.quiet_threshold
         -> 
            Escape % ("start->quite",t,x,y,z)
            F (quite cfg t (F(Twist.start Twist.TwistCfg.Default,None)),None) 
    | _ 
        -> 
            F (start cfg,None)

and quite cfg t_start twist = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when 
        abs(x) <= cfg.quiet_threshold
        && abs(y) <= cfg.quiet_threshold
        && abs(z) <= cfg.quiet_threshold
        && t - t_start > cfg.ts_quite
        -> 
            Escape % ("quite->quite2")
            F (quite2 cfg t_start twist,None)

    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when 
        abs(x) <= cfg.quiet_threshold
        && abs(y) <= cfg.quiet_threshold
        && abs(z) <= cfg.quiet_threshold
        -> 
            F (quite cfg t_start twist,None)

    | {Snsr=LinearAcceleration} 
        -> 
            Escape % ("quite->start")
            F (start cfg, None)

    | s 
        ->
            F (quite cfg t_start twist, None)

and quite2 cfg t_start twist = function
    | {Snsr=Gyroscope;Ticks=t;X=x;Y=y;Z=z} when 
        abs(x) >= cfg.twistcfg.x_threshold
        && abs(y) < cfg.twistcfg.yz_threshold
        && abs(z) < cfg.twistcfg.yz_threshold
        -> 
            Escape % ("quite->runTwist",t,x,y,z)
            F(runTwist cfg t twist,None)

    | {Snsr=Gyroscope} 
        -> 
            F (quite2 cfg t_start twist,None)
     
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when 
        abs x > cfg.quiet_threshold
        && abs y > cfg.quiet_threshold
        && abs z > cfg.quiet_threshold
        ->
            Escape % ("quite2->start",x,y,z)
            F(start cfg, None)
     | _
        ->
            F(quite2 cfg t_start twist, None)

and runTwist cfg t_start (F(twist,_)) = function
    | {Snsr=Gyroscope} as e
        ->
            match twist e with
            | F(_,Some RE_Twist) -> F(start cfg, Some RE_Escape)
            | F(twist, None)     -> F(runTwist cfg t_start (F(twist,None)), None)
            | _                  -> failwith "this should not happen"
    | {Ticks=t} when
        t - t_start > cfg.twistcfg.ts_threshold
        -> 
            F(start cfg,None)
    | _ 
        ->
            F(runTwist cfg t_start (F(twist,None)),None)

*)
