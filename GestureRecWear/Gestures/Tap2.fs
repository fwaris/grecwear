module Tap2
open FSM
open Recognition

type TapCfg =
    {
        mutable gstr_time_limit                 : int64

        mutable time_limit_to_reach_low_accel   : int64
        mutable low_y_accel_limit               : float32

        mutable x_accel_tolerance               : float32
        mutable z_accel_tolerance               : float32

        mutable ret_x_accel_tolerance           : float32
        mutable ret_z_accel_tolerance           : float32

        mutable xy_rot_tolerance                : float32
        mutable z_rot_tolerance                 : float32

    }
//    with
//    static member Default =
//        {
//            gstr_time_limit                 = 700000000L
//
//            time_limit_to_reach_low_accel   = 400000000L
//            low_y_accel_limit               = 0.4f
//
//            x_accel_tolerance               = 2.1f
//            z_accel_tolerance               = 2.1f
//
//            ret_x_accel_tolerance           = 3.1f
//            ret_z_accel_tolerance           = 2.1f
//
//            xy_rot_tolerance                = 1.0f
//            z_rot_tolerance                 = 2.5f
//
//        }

let rec start cfg dir avgyLA t_start returnState = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when //accel in same dir
        t - t_start < cfg.gstr_time_limit
        && abs x < cfg.x_accel_tolerance
        && abs z < cfg.z_accel_tolerance
        && sign y = dir
        ->
        let avgyLA = updateAvg avgyLA y
        Tap % ("avgyLA",avgyLA)
        Tap % ("start->start",t-t_start,x,y,z)
        F(start cfg dir avgyLA  t_start returnState, None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when //accel dir changed
        t - t_start < cfg.gstr_time_limit
        && abs x < cfg.x_accel_tolerance
        && abs z < cfg.z_accel_tolerance
        ->
        let avgyLA = updateAvg avgyLA y
        Tap % ("avgyLA",avgyLA)
        Tap % ("start->seekzero",t-t_start,x,y,z)
        F(seekZeroAvg cfg dir avgyLA t returnState, None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z}
        ->
        Tap % ("start->return",t-t_start,x,y,z)
        F(returnState, None)
    | {Snsr=Gyroscope;X=x;Y=y;Z=z} when
        abs x > cfg.xy_rot_tolerance
        && abs y > cfg.xy_rot_tolerance
        && abs z > cfg.z_rot_tolerance
        ->
        Tap % ("quite->return:rot",x,y,z)
        F(returnState, None)
    | _ ->
        F(start cfg dir avgyLA t_start returnState, None)

and seekZeroAvg cfg dir avgyLA t_start returnState = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when 
        t - t_start < cfg.time_limit_to_reach_low_accel
        && abs x < cfg.ret_x_accel_tolerance
        && abs z < cfg.ret_z_accel_tolerance
        ->
        let avgyLA = updateAvg avgyLA y
        let (curAvgY,_) = avgyLA
        if abs curAvgY < cfg.low_y_accel_limit then
            F(returnState, Some RE_Tap)
        else
            F(seekZeroAvg cfg dir avgyLA t_start returnState, None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z}
        ->
        Tap % ("seekzero->return",t-t_start,x,y,z)
        F(returnState, None)
    | {Snsr=Gyroscope;X=x;Y=y;Z=z} when
        abs x > cfg.xy_rot_tolerance
        && abs y > cfg.xy_rot_tolerance
        && abs z > cfg.z_rot_tolerance
        ->
        Tap % ("seekzero->return:rot",x,y,z)
        F(returnState, None)
    | _ ->
        F( seekZeroAvg cfg dir avgyLA t_start returnState, None)
