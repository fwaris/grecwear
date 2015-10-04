module Swipe
open FSM
open Recognition

type SwipeCfg =
    {
        mutable gstr_time_limit          : int64


        mutable x_accel_tolerance  : float32
        mutable y_accel_tolerance  : float32

        mutable ret_x_accel_tolerance  : float32
        mutable ret_y_accel_tolerance  : float32

        mutable xz_rot_tolerance    : float32
        mutable y_rot_tolerance     : float32

        mutable low_z_accel_limit : float32
    }
//    with
//    static member Default =
//        {
//            gstr_time_limit         = 600000000L
//
//            x_accel_tolerance       = 2.1f
//            y_accel_tolerance       = 2.1f
//
//            ret_x_accel_tolerance   = 2.1f
//            ret_y_accel_tolerance   = 3.1f
//
//            xz_rot_tolerance        = 0.1f
//            y_rot_tolerance         = 2.5f
//
//            low_z_accel_limit       = 0.3f
//        }

let updateAvg (avgzLA,count) y = 
    let count' = (count + 1.f)
    let avgyLA = (avgzLA*count + y) / count'
    (avgyLA,count')

let rec start cfg dir avgzLA t_start returnState = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when //accel in same dir
        t - t_start < cfg.gstr_time_limit
        && abs x < cfg.x_accel_tolerance
        && abs y < cfg.y_accel_tolerance
        && sign z = dir
        ->
        let avgzLA = updateAvg avgzLA z
        Swipe % ("avgzLA",avgzLA)
        Swipe % ("start->start",t-t_start,x,y,z)
        F(start cfg dir avgzLA  t_start returnState, None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when //accel dir changed
        t - t_start < cfg.gstr_time_limit
        && abs x < cfg.x_accel_tolerance
        && abs y < cfg.y_accel_tolerance
        ->
        let avgzLA = updateAvg avgzLA z
        Swipe % ("avgyLA",avgzLA)
        Swipe % ("start->seekzero",t-t_start,x,y,z)
        F(seekZeroAvg cfg dir avgzLA t_start returnState, None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z}
        ->
        Swipe % ("start->return",t-t_start,x,y,z)
        F(returnState, None)
    | {Snsr=Gyroscope;X=x;Y=y;Z=z} when
        abs x > cfg.xz_rot_tolerance
        && abs z > cfg.xz_rot_tolerance
        && abs y > cfg.y_rot_tolerance
        ->
        Swipe % ("quite->return:rot",x,y,z)
        F(returnState, None)
    | _ ->
        F(start cfg dir avgzLA t_start returnState, None)

and seekZeroAvg cfg dir avgzLA t_start returnState = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when 
        t - t_start < cfg.gstr_time_limit
        && abs x < cfg.ret_x_accel_tolerance
        && abs y < cfg.ret_y_accel_tolerance
        ->
        let avgzLA = updateAvg avgzLA z
        let (curAvgZ,_) = avgzLA
        if abs curAvgZ < cfg.low_z_accel_limit then
            Swipe % ("avgzLA",t-t_start,avgzLA,x,y,z)
            F(returnState, Some RE_Swipe)
        else
            F(seekZeroAvg cfg dir avgzLA t_start returnState, None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z}
        ->
        Swipe % ("seekzero->return",t-t_start,avgzLA,x,y,z)
        F(returnState, None)
    | {Snsr=Gyroscope;X=x;Y=y;Z=z} when
        abs x > cfg.xz_rot_tolerance
        && abs z > cfg.xz_rot_tolerance
        && abs y > cfg.y_rot_tolerance
        ->
        Swipe % ("seekzero->return:rot",x,y,z)
        F(returnState, None)
    | {Snsr=Gyroscope;X=x;Y=y;Z=z}
        ->
        Swipe % ("seekzero->return:rot",x,y,z)
        F( seekZeroAvg cfg dir avgzLA t_start returnState, None)
    | _ ->
        F( seekZeroAvg cfg dir avgzLA t_start returnState, None)

