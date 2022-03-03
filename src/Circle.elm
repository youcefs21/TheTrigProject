module Circle exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

myShapes model = [
    unitCircle model.showCast model.showSAngles model.radians,
    triangle model.angle model.quad model.showSLengths
    ]
  
unitCircle showCast showSAngles radians = group [
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
        |> outlined (solid 0.5) blue,

    -- CAST
    if showCast then cast else group [],

    -- Special Angles
    if showSAngles then angles radians else group []
    ]

cast =
    let 
        r = 50
        tText t = text t |> sansserif |> centered |> size 10 |> filled black
    in 
    group [
        tText "C"
            |> move (r, -r),
        tText "A"
            |> move (r, r - 5),
        tText "S"
            |> move (-r, r - 5),
        tText "T"
            |> move (-r, -r)
    ]

angles radians = group <| 
    List.map 
        (\(d, r) -> 
            group [ text (if radians then r else (String.fromFloat d))
                |> sansserif
                |> size 5
                |> centered 
                |> filled black 
                |> makeTransparent 0.5
                |> move 
                    (let
                        x = xpos (ur + 7.5) d
                        y = ypos (ur + 7.5) d
                    in (x, if y > 0 then y - 5 else y)),
            circle 0.5 
                |> filled black
                |> move (pos ur d) 
            ]
            |> notifyTap (UpdateAngle d)) 
        specialAngles
    

triangle angle quad showSLengths = group [
    -- Alpha angle
    group [
        let
            alpha = case quad of
                One -> angle
                Two -> 180 - angle
                Three -> angle - 180
                Four -> 360 - angle

        in
            if angle == 0 then group [] else
            if angle == 90 || angle == 270 then 
                square 3
                    |> outlined (solid 0.5) darkGrey
                    |> move (1.5, 1.5)
                    |> scaleX (if angle == 90 then 1 else -1)
                    |> scaleY (if angle == 90 then 1 else -1)
            else
                wedge 3 (alpha / 360)
                    |> outlined (solid 0.5) darkGrey
                    |> rotate (degrees (alpha / 2))
                    |> scaleX (if quad == Two   || quad == Three then -1 else 1)
                    |> scaleY (if quad == Three || quad == Four  then -1 else 1)
    ],

    -- Hypothenuse
    line org (pos ur angle)
        |> outlined (solid 0.5) red,
    
    -- Adjacent
    line org (xpos ur angle, 0)
        |> outlined (dotted 0.5) blue,

    -- Opposite
    line (xpos ur angle, 0) (pos ur angle)
        |> outlined (dotted 0.5) green,

    -- Side Lengths

    -- Dot
    circle 1
        |> filled red
        |> move (pos ur angle)
    ]

ur = 50
org = (0, 0)
xpos r angle = r * cos (degrees angle)
ypos r angle = r * sin (degrees angle)
pos  r angle = (xpos r angle, ypos r angle)

-- Angles are shown as (degrees as Float, radians as String)
-- everything is technically done through degrees
specialAngles = [(0,   "0 (2π)"),    (30,  "π/6"),   (45,  "π/4"),  (60,  "π/3"), 
                 (90,  "π/2"),  (120, "2π/3"),  (135, "3π/4"), (150, "5π/6"),
                 (180, "π"),    (210, "7π/6π"), (225, "5π/4"), (240, "4π/3"),
                 (270, "3π/2"), (300, "5π/3"),  (315, "7π/4"), (330, "11π/6")]

limitAngle angle = 
    if angle > 360 then limitAngle (angle - 360) else angle

type Quadrant = One 
              | Two 
              | Three 
              | Four

updateQuad angle =
    if angle > 0   && angle <= 90  then One   else
    if angle > 90  && angle <= 180 then Two   else
    if angle > 180 && angle <= 270 then Three else
                                        Four

type Msg = Tick Float GetKeyState
         | UpdateAngle Float

type alias Model = { 
    time         : Float,
    angle        : Float,
    quad         : Quadrant,
    showCast     : Bool,
    showSAngles  : Bool,
    showSLengths : Bool,
    radians      : Bool }

init = { 
    time         = 0,
    angle        = 0,
    quad         = One,
    showCast     = True,
    showSAngles  = True,
    showSLengths = True,
    radians      = True }

update msg model = 
    case msg of
        Tick t _ -> 
            ({ model | time = t }, Cmd.none)
        UpdateAngle newAngle -> 
            let 
                newNewAngle = limitAngle newAngle
            in
                ({ model | angle = newNewAngle,
                           quad  = updateQuad newNewAngle }, Cmd.none)

main : EllieAppWithTick () Model Msg
main = ellieAppWithTick Tick { 
    init = \_ -> ( init, Cmd.none ), 
    update = update, 
    view = \model -> { title = "Unit Circle", body = view model },
    subscriptions = \_ -> Sub.none }

view model = collage 192 128 (myShapes model)



