//recognize escape gesture
module Navigation
open Recognition
open FSM

type NavigationCfg = 
    {
        mutable xyz_accel_quite_limit  : float32
        mutable TapConfig              : PreTap.FRCfg
        mutable LRConfig               : Left_Right.LRConfig
        mutable SwipeConfig            : PreSwipe.PreSwipeCfg
    }
    with
    static member Default =
//        {
//            xyz_accel_quite_limit  = 0.5f
//            TapConfig = 
//                    {
//                        gstr_time_limit             = 100000000L
//
//                        y_accel_front_thrld         = 1.7f
//                        y_accel_back_thrld          = -1.7f
//                        y_accel_avg_front_thrld     = 1.5f
//                        y_accel_avg_back_thrld      = -1.5f
//                        avg_over_count              = 4.0f
//
//                        xz_accel_tolerance          = 0.5f
//
//                        xy_rot_tolerance            = 1.0f
//                        z_rot_tolerance             = 2.7f
//
//                        TapCfg =
//                            {
//                                gstr_time_limit                 = 700000000L
//
//                                time_limit_to_reach_low_accel   = 400000000L
//                                low_y_accel_limit               = 0.4f
//
//                                x_accel_tolerance               = 2.1f
//                                z_accel_tolerance               = 2.1f
//
//                                ret_x_accel_tolerance           = 3.1f
//                                ret_z_accel_tolerance           = 2.1f
//
//                                xy_rot_tolerance                = 1.0f
//                                z_rot_tolerance                 = 2.5f
//
//                            }
//                    }
//            LRConfig = 
//                    {
//                        x_grav_high_limit       = 8.5f
//                        x_grav_low_limit        = 2.0f
//        
//                        z_grav_zero_tolerance    = 3.0f
//                        z_grav_Left_thrsld       = -4.0f
//                        z_grav_right_thrsld      = 6.f
//                    }
//            SwipeConfig =
//                    {
//                        gstr_time_limit             = 100000000L
//
//                        z_accel_front_thrsld        = 1.7f
//                        z_accel_back_thrsld         = -1.7f
//                        z_accel_avg_front_thrsld    = 1.5f
//                        z_accel_avg_back_thrsld     = -1.5f
//
//                        xy_accel_tolerance          = 0.5f
//
//                        avg_over_count              = 4.0f
//
//                        xz_rot_tolerance            = 0.1f
//                        y_rot_tolerance             = 2.5f
//
//                        SwipeCfg =
//                            {
//                                gstr_time_limit         = 600000000L
//
//                                x_accel_tolerance       = 2.1f
//                                y_accel_tolerance       = 2.1f
//
//                                ret_x_accel_tolerance   = 2.1f
//                                ret_y_accel_tolerance   = 3.1f
//
//                                xz_rot_tolerance        = 0.1f
//                                y_rot_tolerance         = 2.5f
//
//                                low_z_accel_limit       = 0.3f
//                            }
//                    }
//        }
      {xyz_accel_quite_limit = 1.79598546f;
       TapConfig = {gstr_time_limit = 763325877L;
                    y_accel_front_thrld = 4.59508657f;
                    y_accel_back_thrld = -4.99988461f;
                    y_accel_avg_front_thrld = 0.947857618f;
                    y_accel_avg_back_thrld = -4.8693471f;
                    xz_accel_tolerance = 1.7438271f;
                    avg_over_count = 3.0f;
                    xy_rot_tolerance = 0.405561686f;
                    z_rot_tolerance = 4.69138145f;
                    TapCfg = {gstr_time_limit = 583838733L;
                              time_limit_to_reach_low_accel = 389325463L;
                              low_y_accel_limit = 2.52411532f;
                              x_accel_tolerance = 3.59369946f;
                              z_accel_tolerance = 1.93497658f;
                              ret_x_accel_tolerance = 3.33933735f;
                              ret_z_accel_tolerance = 2.90307617f;
                              xy_rot_tolerance = 2.73816824f;
                              z_rot_tolerance = 1.37228799f;};};
       LRConfig = {x_grav_high_limit = 7.05250692f;
                   x_grav_low_limit = 0.867585123f;
                   z_grav_zero_tolerance = 0.138050973f;
                   z_grav_Left_thrsld = -4.60895205f;
                   z_grav_right_thrsld = 7.01641941f;};
       SwipeConfig = {z_accel_front_thrsld = 2.74647951f;
                      z_accel_back_thrsld = -4.25041485f;
                      z_accel_avg_front_thrsld = 2.39706802f;
                      z_accel_avg_back_thrsld = -4.60432911f;
                      xy_accel_tolerance = 2.69525146f;
                      avg_over_count = 1.0f;
                      gstr_time_limit = 781282632L;
                      xz_rot_tolerance = 2.24759841f;
                      y_rot_tolerance = 3.57676268f;
                      SwipeCfg = {gstr_time_limit = 460464225L;
                                  x_accel_tolerance = 3.22107768f;
                                  y_accel_tolerance = 2.86686087f;
                                  ret_x_accel_tolerance = 2.9499104f;
                                  ret_y_accel_tolerance = 3.1444633f;
                                  xz_rot_tolerance = 2.97020698f;
                                  y_rot_tolerance = 0.526834428f;
                                  low_z_accel_limit = 3.09724712f;};};}

//let initTap   = F(PreTap.start PreTap.FRCfg.Default,None)
//let initFB    = F(Left_Right.start Left_Right.LRConfig.Default,None)
//let initSwipe = F(PreSwipe.start PreSwipe.PreSwipeCfg.Default,None)



let rec initialQuite cfg = function 
    | {Snsr=LinearAcceleration;Ticks=t;X=x;Y=y;Z=z} when //sensors are unsettled after a twist gesture, wait for quite
        abs x < cfg.xyz_accel_quite_limit 
        && abs y < cfg.xyz_accel_quite_limit 
        && abs z < cfg.xyz_accel_quite_limit 
        ->
        Nav % ("initialQuite->start",x,y,z)
        let initTap   = F(PreTap.start cfg.TapConfig,None)
        let initFB    = F(Left_Right.start cfg.LRConfig,None)
        let initSwipe = F(PreSwipe.start cfg.SwipeConfig,None)
        F(start [initFB; initTap; initSwipe], None)
    | {Snsr=LinearAcceleration;X=x;Y=y;Z=z}
        ->
        //printfn "LA; x=%f;y=%f;z=%f" x y z
        //Ready % ("initialQuite->initialQuite",s,x,y,z)
        F(initialQuite cfg, None)
    | {Snsr=s;X=x;Y=y;Z=z}
        ->
        //printfn "s=%d; x=%f;y=%f;z=%f" s x y z
        //Ready % ("initialQuite->initialQuite",s,x,y,z)
        F(initialQuite cfg, None)

and start states event =                                    //runs left-right and front-back recognition in parallel
    match evalStates states event with
    | newstates,Some ev -> F(start newstates, Some ev)
    | newstates,None    -> F(start newstates, None)

