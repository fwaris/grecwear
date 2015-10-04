module PreTap
open Recognition
open FSM

type FRCfg = 
    {

        mutable gstr_time_limit             : int64
        mutable y_accel_front_thrld         : float32
        mutable y_accel_back_thrld          : float32
        mutable y_accel_avg_front_thrld     : float32
        mutable y_accel_avg_back_thrld      : float32
        mutable xz_accel_tolerance          : float32

        mutable avg_over_count              : float32


        mutable xy_rot_tolerance            : float32
        mutable z_rot_tolerance             : float32

        mutable TapCfg                      : Tap2.TapCfg

    }
//    with
//    static member Default =
//        {
//            gstr_time_limit             = 100000000L
//
//            y_accel_front_thrld         = 1.7f
//            y_accel_back_thrld          = -1.7f
//            y_accel_avg_front_thrld     = 1.5f
//            y_accel_avg_back_thrld      = -1.5f
//            avg_over_count              = 4.0f
//
//            xz_accel_tolerance          = 0.5f
//
//            xy_rot_tolerance            = 1.0f
//            z_rot_tolerance             = 2.7f
//        }

let rec start cfg = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when 
        y > cfg.y_accel_front_thrld 
        || y < cfg.y_accel_back_thrld
        && abs x < cfg.xz_accel_tolerance
        && abs z < cfg.xz_accel_tolerance
        ->
        PreTap % ("->front_or_back",x,y,z)
        F(front_or_back cfg t (y,1.f), None)
    | _ 
        ->
        F(start cfg,None)

and front_or_back cfg t_start ((_,count) as avgyLA) = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when 
        t - t_start < cfg.gstr_time_limit
        && count > cfg.avg_over_count
        ->
        let avgyLA = updateAvg avgyLA y
        let curYAvg,_ = avgyLA
        PreTap % ("avgyLA",avgyLA)
        if curYAvg <  cfg.y_accel_avg_back_thrld then
            PreTap % ("->Tap back",t-t_start,x,y,z)
            F(Tap2.start cfg.TapCfg (sign curYAvg) avgyLA t_start (start cfg), None)
        elif curYAvg > cfg.y_accel_avg_front_thrld then
            PreTap % ("->Tap front",t-t_start,x,y,z)
            F(Tap2.start cfg.TapCfg (sign curYAvg) avgyLA t_start (start cfg), None)
        else
            F(front_or_back cfg  t_start avgyLA,None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when
        t - t_start < cfg.gstr_time_limit
        && count <= cfg.avg_over_count
        ->
        let avgyLA = updateAvg avgyLA y
        PreTap % ("avgyLA",avgyLA, y)
        F(front_or_back cfg t_start avgyLA,None)
    | {Ticks=t} when
        t - t_start >= cfg.gstr_time_limit
        ->
        PreTap % ("->start",t - t_start)
        F(start cfg ,None)
    | {Snsr=Gyroscope;X=x;Y=y;Z=z} when
        abs x > cfg.xy_rot_tolerance
        && abs y > cfg.xy_rot_tolerance
        && abs z > cfg.z_rot_tolerance
        ->
        PreTap % ("->start:rot",x,y,z)
        F(start cfg, None)
    | _
        ->
        F(front_or_back cfg t_start avgyLA, None)
