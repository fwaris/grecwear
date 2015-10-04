module SensorTest
open Recognition
open FSM
//state machine that just prints out sensor data 
//used for debugging only

let rec start = function 
    | {Snsr=LinearAcceleration;X=x;Y=y;Z=z} -> ST_LA % ("LA",x,y,z);    F(start, None)
    | {Snsr=Gravity;X=x;Y=y;Z=z}            -> ST_GR % ("Gr",x,y,z);    F(start, None)
    | {Snsr=Gyroscope;X=x;Y=y;Z=z}          -> ST_GY % ("Gy",x,y,z);    F(start, None)
    | {Snsr=s;X=x;Y=y;Z=z}                  -> ST % ("other",s,x,y,z);  F(start, None)
