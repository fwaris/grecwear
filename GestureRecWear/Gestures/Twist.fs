//recognize twist gesture
module Twist
open Recognition
open FSM

type TwistCfg = 
    {
        mutable x_rot_threshold     : float32
        mutable yz_rot_tolerance    : float32
        mutable gstr_time_limit     : int64   //time limit for the twist gesture to complete in
    }
    with 
    static member Default = {
        gstr_time_limit     = 400000000L 
        x_rot_threshold     = 3.5f
        yz_rot_tolerance    = 1.1f
        }

let negative cfg x = x <= -cfg.x_rot_threshold
let positive cfg x = x >= cfg.x_rot_threshold
let next x = if x > 0.f then negative else positive

let rec start cfg = function 
    | {Ticks=t;X=x;Y=y;Z=z} when 
        abs(x) >= cfg.x_rot_threshold
        && abs(y) < cfg.yz_rot_tolerance
        && abs(z) < cfg.yz_rot_tolerance
        -> 
            Twist % ("start->track",t,x,y,z)
            F (track cfg (next x cfg) t,None) 

    | _ 
        -> 
            F (start cfg,None)

and track cfg next t_start = function
    | {Ticks=t;X=x;Y=y;Z=z} when 
        next x
        && abs(y) < cfg.yz_rot_tolerance
        && abs(z) < cfg.yz_rot_tolerance
        && t - t_start < cfg.gstr_time_limit
        -> 
            Twist % ("track->trackEnd",t,x,y,z)
            F (trackEnd cfg t_start,None) 

    | {Ticks=t;X=x;Y=y;Z=z} when 
        abs(y) < cfg.yz_rot_tolerance
        && abs(z) < cfg.yz_rot_tolerance
        && t - t_start < cfg.gstr_time_limit
        -> 
            F (track cfg next t_start,None)

    | {Snsr=s;Ticks=t;X=x;Y=y;Z=z} 
        ->  
            Twist % ("track -> abandon twist",s,t,x,y,z)
            F (start cfg,None)

and trackEnd cfg t_start = function
    | {Ticks=t;X=x;Y=y;Z=z} when 
        x < cfg.yz_rot_tolerance
        && abs(y) < cfg.yz_rot_tolerance
        && abs(z) < cfg.yz_rot_tolerance
        && t - t_start < cfg.gstr_time_limit
        -> 
            Twist % ("RE_Twist",t,x,y,z)
            F(start cfg,Some RE_Twist)
             
    | {Ticks=t;X=x;Y=y;Z=z} 
        ->  
            Twist % ("trackEnd -> abandon twist",t,x,y,z)
            F (start cfg,None)

