module Circle exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Consts exposing (..)

fonts = {
    monospace = "Consolas",
    sansserif = "Arial"
    }

lightTheme = {
    adj = blue,
    opp = green,
    hyp = red,
    tan = rgb 0 128 128,
    cir = lightBlue }

myShapes model = [
    group [
        unitCircle model.col model.showCast model.showSAngles model.radians model.angle,
        triangle model.col model.angle model.quad model.showSLengths
    ]
        |> scale 0.75
    ]
  
unitCircle col showCast showSAngles radians angle = group [
    -- Grid
    group [
        rect 0.5 110
            |> filled black,
        rect 110 0.5
            |> filled black 
        ]
        |> makeTransparent 0.3,

    -- Circle
    circle 50 
        |> outlined (solid 0.5) col.cir
        |> makeTransparent 0.7,

    -- CAST
    if showCast then cast col else group [],

    -- Special Angles
    if showSAngles then angles radians angle else group []
    ]

cast col =
    let 
        r = 50
        tText t c = text t |> customFont fonts.sansserif |> centered |> size 10 |> filled c
    in 
    group [
        tText "C" col.adj
            |> move (r, -r),
        tText "A" col.hyp
            |> move (r, r - 5),
        tText "S" col.opp
            |> move (-r, r - 5),
        tText "T" col.tan
            |> move (-r, -r)
    ]

angles radians angle = group <| 
    List.map 
        (\(d, r) -> 
            let 
                str      = if radians then r else (String.fromFloat d)
                strlen s = (*) s <| toFloat <| String.length str
                quad     = updateQuad d
                x = xpos (ur + 7.5) d + if quad == One || quad == Four then strlen 1.5 else -(strlen 1.5)
                y = ypos (ur + 7.5) d + if quad == One || quad == Two  then -3 else 0
            in
                group [ 
                    rect (strlen 3) 8
                        |> ghost
                        |> move (x, y + 1),
                    text str
                        |> customFont fonts.monospace
                        |> size 4
                        |> centered 
                        |> (if angle == d then bold else identity)
                        |> filled black 
                        |> makeTransparent 0.5
                        |> move (x, y),
                    circle 0.5 
                        |> filled black
                        |> move (pos ur d) 
                ]
                |> notifyTap (UpdateAngle d)) 
        specialAngles
    

triangle col angle quad showSLengths =
    let
        alpha = case quad of
                One -> angle
                Two -> 180 - angle
                Three -> angle - 180
                Four -> 360 - angle
    in group [
        -- Alpha angle
        group [
            if angle == 0 then group [] else
            if angle == 90 || angle == 270 then 
                square 3
                    |> outlined (solid 0.3) darkGrey
                    |> move (1.5, 1.5)
                    |> scaleX (if angle == 90 then 1 else -1)
                    |> scaleY (if angle == 90 then 1 else -1)
            else
                wedge 3 (alpha / 360)
                    |> outlined (solid 0.3) darkGrey
                    |> rotate (degrees (alpha / 2))
                    |> scaleX (if quad == Two   || quad == Three then -1 else 1)
                    |> scaleY (if quad == Three || quad == Four  then -1 else 1)
        ]
            |> makeTransparent 0.7,

        -- Full angle
        circle 2 
            |> outlined (solid 0.3) red
            |> makeTransparent 0.6
            |> clip (wedge 5 (angle / 360)
                |> filled red
                |> rotate (degrees (angle / 2))),
        -- Adjacent
        line org (xpos ur angle, 0)
            |> outlined (dotted 0.5) col.adj,

        -- Opposite
        line (xpos ur angle, 0) (pos ur angle)
            |> outlined (dotted 0.5) col.opp,

        -- Hypothenuse
        line org (pos ur angle)
            |> outlined (solid 0.5) col.hyp,

        -- Side Lengths
        if showSLengths then 
            let
                rawAdj = getString alpha adjLengths
                rawOpp = getString alpha oppLengths
                adj = (if quad == Two   || quad == Three && rawAdj /= "0" then "-" else "") ++ rawAdj
                opp = (if quad == Three || quad == Four  && rawOpp /= "0" then "-" else "") ++ rawOpp
                tText str c = 
                    text str
                        |> customFont "Consolas"
                        |> centered
                        |> size 3
                        |> filled c
                    
            in
                group [
                    -- Adj
                    tText adj col.adj
                        |> move (xpos (ur / 2) angle, 
                                 if quad == One || quad == Two then -4
                                 else 1.5),
                    -- Opp
                    tText opp col.opp
                        |> move (xpos ur angle +
                                 if quad == One || quad == Four then (toFloat (String.length opp) + 1)
                                 else -(toFloat (String.length opp) + 1), ypos (ur / 2) angle),
                    -- Hyp
                    tText "1" col.hyp
                        |> move (xpos (ur / 2) angle +
                                 if quad == One || quad == Four then -3 else 3, 
                                 ypos (ur / 2) angle + 
                                 if quad == One || quad == Two then 3 else -5)
                ]  
        else group [],

        -- Dot
        circle 1
            |> filled col.hyp
            |> move (pos ur angle)
        ]

ur = 50
org = (0, 0)
xpos r angle = r * cos (degrees angle)
ypos r angle = r * sin (degrees angle)
pos  r angle = (xpos r angle, ypos r angle)

-- Angles are shown as (degrees as Float, radians as String)
-- everything is technically done through degrees
specialAngles = [
    (0,   "0 (2π)"), (30,  "π/6"),   (45,  "π/4"),  (60,  "π/3"), 
    (90,  "π/2"),    (120, "2π/3"),  (135, "3π/4"), (150, "5π/6"),
    (180, "π"),      (210, "7π/6"),  (225, "5π/4"), (240, "4π/3"),
    (270, "3π/2"),   (300, "5π/3"),  (315, "7π/4"), (330, "11π/6")]
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


type alias Model = { 
    time         : Float,
    angle        : Float,
    quad         : Quadrant,
    showCast     : Bool,
    showSAngles  : Bool,
    showSLengths : Bool,
    radians      : Bool,
    col          : { adj : Color, opp : Color, hyp : Color, tan : Color, cir : Color }}

init = { 
    time         = 0,
    angle        = 30,
    quad         = One,
    showCast     = True,
    showSAngles  = True,
    showSLengths = True,
    radians      = True,
    col          = lightTheme }
update : Consts.Msg -> Model -> Model
update msg model = 
    case msg of
        Tick t _ -> 
            { model | time = t }
        UpdateAngle newAngle -> 
            let 
                newNewAngle = limitAngle newAngle
            in
                { model | angle = newNewAngle,
                           quad  = updateQuad newNewAngle }
        -- _ -> model

main = gameApp Tick {
    model = init,
    view = view,
    update = update,
    title = "Unit Circle" 
    }

view model = collage 192 128 (myShapes model)



