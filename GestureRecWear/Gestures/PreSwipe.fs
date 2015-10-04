module PreSwipe
open Recognition
open FSM

type PreSwipeCfg = 
    {

        mutable z_accel_front_thrsld : float32
        mutable z_accel_back_thrsld  : float32
        mutable z_accel_avg_front_thrsld   : float32
        mutable z_accel_avg_back_thrsld    : float32

        mutable xy_accel_tolerance      : float32

        mutable avg_over_count      : float32

        mutable gstr_time_limit              : int64

        mutable xz_rot_tolerance    : float32
        mutable y_rot_tolerance     : float32

        mutable SwipeCfg            : Swipe.SwipeCfg

    }
//    with
//    static member Default =
//        {
//            gstr_time_limit             = 100000000L
//
//            z_accel_front_thrsld        = 1.7f
//            z_accel_back_thrsld         = -1.7f
//            z_accel_avg_front_thrsld    = 1.5f
//            z_accel_avg_back_thrsld     = -1.5f
//
//            xy_accel_tolerance          = 0.5f
//
//            avg_over_count              = 4.0f
//
//            xz_rot_tolerance            = 0.1f
//            y_rot_tolerance             = 2.5f
//        }

let rec start cfg = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when 
        z > cfg.z_accel_front_thrsld 
        || z < cfg.z_accel_back_thrsld
        && abs x < cfg.xy_accel_tolerance
        && abs y < cfg.xy_accel_tolerance
        ->
        PreSwipe % ("->side_to_side",x,y,z)
        F(side_to_side cfg t (z,1.f), None)
    | _ 
        ->
        F(start cfg,None)

and side_to_side cfg t_start ((_,count) as avgzLA) = function
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when 
        t - t_start < cfg.gstr_time_limit
        && count > cfg.avg_over_count
        ->
        let avgzLA = updateAvg avgzLA z
        let curZAvg,_ = avgzLA
        PreSwipe % ("avgzLA",avgzLA)
        if curZAvg <  cfg.z_accel_avg_back_thrsld then
            PreSwipe % ("->Swipe right",t-t_start,x,y,z)
            F(Swipe.start cfg.SwipeCfg (sign curZAvg) avgzLA t_start (start cfg), None)
        elif curZAvg > cfg.z_accel_avg_front_thrsld then
            PreSwipe % ("->Swipe left",t-t_start,x,y,z)
            F(Swipe.start cfg.SwipeCfg (sign curZAvg) avgzLA t_start (start cfg), None)
        else
            F(side_to_side cfg  t_start avgzLA,None)
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when
        t - t_start < cfg.gstr_time_limit
        && count <= cfg.avg_over_count
        ->
        let avgzLA = updateAvg avgzLA z
        PreSwipe % ("avgzLA",avgzLA, z)
        F(side_to_side cfg t_start avgzLA,None)
    | {Ticks=t} when
        t - t_start >= cfg.gstr_time_limit
        ->
        PreSwipe % ("->start",t - t_start)
        F(start cfg ,None)
    | {Snsr=Gyroscope;X=x;Y=y;Z=z} when
        abs x > cfg.xz_rot_tolerance
        && abs z > cfg.xz_rot_tolerance
        && abs y > cfg.y_rot_tolerance
        ->
        PreSwipe % ("->start:rot",x,y,z)
        F(start cfg, None)
    | _
        ->
        F(side_to_side cfg t_start avgzLA, None)

