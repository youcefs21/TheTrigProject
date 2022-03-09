module Consts exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


type Msg = Tick Float GetKeyState
            -- Circle Message
         | UpdateAngle Float
            -- Graphing Message
         | SetFunc Int Func
         | SetCol Theme

type Func = Sin
          | Cos
          | Tan

type Theme = Light
           | Dark

-- Angles are shown as (degrees as Float, radians as String)
-- everything is technically done through degrees
specialAngles = [
    (0,   "0"),      (30,  "π/6"),   (45,  "π/4"),  (60,  "π/3"), 
    (90,  "π/2"),    (120, "2π/3"),  (135, "3π/4"), (150, "5π/6"),
    (180, "π"),      (210, "7π/6"),  (225, "5π/4"), (240, "4π/3"),
    (270, "3π/2"),   (300, "5π/3"),  (315, "7π/4"), (330, "11π/6"),
    (360, "2π")]
adjLengths = [(0, "1"), (30, "(√3)/2"), (45, "1/(√2)"), (60, "1/2"),    (90, "0")]
oppLengths = [(0, "0"), (30, "1/2"),    (45, "1/(√2)"), (60, "(√3)/2"), (90, "1")]

getString i xss = 
    case xss of
        [] -> "ERR"
        ((x1, x2)::xs) -> if i == x1 then x2 else getString i xs

limitAngle angle = 
    if angle > 360 then limitAngle (angle - 360) else angle

type Quadrant = One 
              | Two 
              | Three 
              | Four

updateQuad angle =
    if angle >= 0  && angle <= 90  then One   else
    if angle > 90  && angle <= 180 then Two   else
    if angle > 180 && angle <= 270 then Three else
                                        Four

getTheme t = 
    case t of
        Light -> lightTheme
        Dark  -> darkTheme

getCol f t = 
    case f of
        Sin -> t.opp
        Cos -> t.adj
        Tan -> t.tan

getFunc f = 
    case f of
        Sin -> sin
        Cos -> cos
        Tan -> tan

fonts = {
    monospace = "Consolas",
    sansserif = "Arial"
    }

lightTheme = {
    adj = blue,
    opp = green,
    hyp = red,
    tan = rgb 0 128 128,
    cir = lightBlue,
    grid       = black,
    buttons    = yellow,
    background = white,
    movingLine = lightGreen }

darkTheme = {
    adj = blue,
    opp = green,
    hyp = red,
    tan = rgb 0 128 128,
    cir = lightBlue,
    grid       = black,
    buttons    = yellow,
    background = white,
    movingLine = lightGreen }