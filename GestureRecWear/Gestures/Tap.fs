module Tap
open FSM
open Recognition

type TapCfg =
    {
        mutable ts_min          : int64
        mutable ts_max          : int64

        mutable quiet_thrsld    : float32
        mutable ts_quite        : int64

        mutable x_accel_thrsld  : float32
        mutable z_accel_thrsld  : float32

        mutable ret_x_accel_thrsld  : float32
        mutable ret_z_accel_thrsld  : float32

        mutable xy_rot_tolerance    : float32
        mutable z_rot_tolerance     : float32
    }
//    with
//    static member Default =
//        {
//            ts_min              = 100000000L 
//            ts_max              = 600000000L
//
//            quiet_thrsld        = 0.3f
//            ts_quite            = 10000000L
//
//            x_accel_thrsld      = 2.1f
//            z_accel_thrsld      = 1.1f
//
//            ret_x_accel_thrsld  = 3.1f
//            ret_z_accel_thrsld  = 2.1f
//
//            xy_rot_tolerance   = 0.1f
//            z_rot_tolerance    = 2.5f
//        }

let rec start cfg dir t_start returnState = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when //accel in same dir
        t - t_start < cfg.ts_max
        && abs x < cfg.x_accel_thrsld
        && abs z < cfg.z_accel_thrsld
        && sign y = dir
        ->
        Tap % ("start->start",t-t_start,x,y,z)
        F(start cfg dir t_start returnState, None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when //accel dir changed
        t - t_start < cfg.ts_max
        && abs x < cfg.x_accel_thrsld
        && abs z < cfg.z_accel_thrsld
        ->
        Tap % ("start->back",t-t_start,x,y,z)
        F(back cfg dir t_start t returnState, None)
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
        F(start cfg dir t_start returnState, None)

and back cfg dir t_start t_startBack returnState = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when //accel in reverse dir for some min time
        t - t_startBack < cfg.ts_min
        && t - t_start < cfg.ts_max
        && abs x < cfg.ret_x_accel_thrsld
        && abs z < cfg.ret_z_accel_thrsld
        && sign y <> dir
        ->
        F(back cfg dir t_start t_startBack returnState, None) //min time passed look for quite
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when 
        t - t_start < cfg.ts_max
        && abs x < cfg.ret_x_accel_thrsld
        && abs z < cfg.ret_z_accel_thrsld
        && sign y <> dir
        ->
        Tap % ("back->seekQuite",t-t_start,x,y,z)
        F(seekQuite cfg dir t_start returnState, None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z}
        ->
        Tap % ("back->return",t-t_start,x,y,z)
        F(returnState, None)
    | {Snsr=Gyroscope;X=x;Y=y;Z=z} when
        abs x > cfg.xy_rot_tolerance
        && abs y > cfg.xy_rot_tolerance
        && abs z > cfg.z_rot_tolerance
        ->
        Tap % ("quite->return:rot",x,y,z)
        F(returnState, None)
    | _ ->
        F(back cfg dir t_start t_startBack returnState, None)

and seekQuite cfg dir t_start returnState = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when //accel is quite now -> wait for min time in quite
        t - t_start < cfg.ts_max
        && abs x < cfg.x_accel_thrsld
        && abs z < cfg.z_accel_thrsld
        && abs y <= cfg.quiet_thrsld
        ->
        Tap % ("seekQuite->return",t-t_start,x,y,z)
        F(returnState, Some RE_Tap)
//        F(quite cfg dir t returnState, None)
    | {Ticks=t} when
        t - t_start < cfg.ts_max
        ->
        F(seekQuite cfg dir t_start returnState, None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z}
        ->
        Tap % ("seekQuite->return",t-t_start,x,y,z)
        F(returnState, None)
    | {Snsr=Gyroscope;X=x;Y=y;Z=z} when
        abs x > cfg.xy_rot_tolerance
        && abs y > cfg.xy_rot_tolerance
        && abs z > cfg.z_rot_tolerance
        ->
        Tap % ("quite->return:rot",x,y,z)
        F(returnState, None)
    | _ ->
        F(seekQuite cfg dir t_start returnState, None)
(*
and quite cfg dir t_startQuiet returnState = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when //pass time in quite mode
        t - t_startQuiet < cfg.ts_quite
        && abs x < cfg.x_accel_thrsld
        && abs z < cfg.z_accel_thrsld
        && abs y < cfg.quiet_thrsld
        ->
        F(quite cfg dir t_startQuiet returnState, None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when //quite for long enough -> recognize event and return
        t - t_startQuiet >= cfg.ts_quite
        && abs x < cfg.x_accel_thrsld
        && abs z < cfg.z_accel_thrsld
        && abs y < cfg.quiet_thrsld
        ->
        let ev = if dir > 0 then RE_Front else RE_Back
        F(returnState, Some ev)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z}
        ->
        Tap % ("quite->return",t-t_startQuiet,x,y,z)
        F(returnState, None)
    | {Snsr=Gyroscope;X=x;Y=y;Z=z} when
        abs x > cfg.xy_rot_tolerance
        && abs y > cfg.xy_rot_tolerance
        && abs z > cfg.z_rot_tolerance
        ->
        Tap % ("quite->return:rot",x,y,z)
        F(returnState, None)
    | _ ->
        F(quite cfg dir t_startQuiet returnState, None)
*)
