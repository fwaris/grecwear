module Left_Right
open Recognition
open FSM

type LRConfig =
    {
        mutable x_grav_high_limit       : float32
        mutable x_grav_low_limit        : float32

        mutable z_grav_zero_tolerance    : float32
        mutable z_grav_Left_thrsld       : float32
        mutable z_grav_right_thrsld      : float32
    }
//    with
//    static member Default =
//        {
//            x_grav_high_limit       = 8.5f
//            x_grav_low_limit        = 2.0f
//
//            z_grav_zero_tolerance    = 3.0f
//            z_grav_Left_thrsld       = -4.0f
//            z_grav_right_thrsld      = 6.f
//        }

let rec start cfg = function
    | {Snsr=Gravity;Ticks=t;X=x;Y=y;Z=z} when  //move to ready when forearm orientation is within range
        x > cfg.x_grav_low_limit 
        && x < cfg.x_grav_high_limit
        && abs z < cfg.z_grav_zero_tolerance 
        ->
        LR % ("start->ready",x,y,z)
        F(ready cfg, None)
    | {Snsr=Gravity;X=x;Y=y;Z=z}
        ->
        //LR % ("start->start","Gr",x,y,z)
        F(start cfg, None)
    | {Snsr=s;X=x;Y=y;Z=z}
        ->
        //LR % ("start->start",s,x,y,z)
        F(start cfg, None)

and ready cfg = function
    | {Snsr=Gravity;Ticks=t;X=x;Y=y;Z=z} when 
        x > cfg.x_grav_low_limit 
        && x < cfg.x_grav_high_limit
        && z < cfg.z_grav_Left_thrsld
        ->
        F(start cfg, Some RE_Left)
    | {Snsr=Gravity;Ticks=t;X=x;Y=y;Z=z} when 
        x > cfg.x_grav_low_limit 
        && x < cfg.x_grav_high_limit
        && z > cfg.z_grav_right_thrsld
        ->
        F(start cfg, Some RE_Right)
    | {Snsr=Gravity;Ticks=t;X=x;Y=y;Z=z} when 
        x > cfg.x_grav_low_limit 
        || x < cfg.x_grav_high_limit
        ->
        F(ready cfg, None)
    | {Snsr=Gravity;Ticks=t;X=x;Y=y;Z=z} 
        -> 
        LR % ("ready->start",x,y,z)
        F(start cfg, None)
    | _
        ->
        F(ready cfg, None)
