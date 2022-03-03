module Circle exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

myShapes model = [
    unitCircle,
    triangle model.angle model.quad
    ]

type Quadrant = One | Two | Three | Four

org = (0, 0)
xpos angle = 50 * cos (degrees angle)
ypos angle = 50 * sin (degrees angle)
pos angle = (xpos angle, ypos angle)

triangle angle quad = group [
    -- Alpha angle
    group [
        let
            alpha = case quad of
                One -> angle
                Two -> 180 - angle
                Three -> angle - 180
                Four -> 360 - angle

        in
            wedge 3 (alpha / 360)
                |> outlined (solid 0.5) darkGrey
                |> rotate (degrees (alpha / 2))
                |> scaleX (if quad == Two || quad == Three then -1 else 1)
                |> scaleY (if quad == Three || quad == Four then -1 else 1)
    ],

    -- Hypothenuse
    line org (pos angle)
        |> outlined (solid 0.5) red,
    
    -- Adjacent
    line org (xpos angle, 0)
        |> outlined (dashed 0.5) blue,

    -- Opposite
    line (xpos angle, 0) (pos angle)
        |> outlined (dashed 0.5) green,

    -- Dot
    circle 1
        |> filled red
        |> move (pos angle)
    ]
  
unitCircle = group [
    -- Grid
    rect 0.5 110
        |> filled grey,
    rect 110 0.5
        |> filled grey,

    -- Circle
    circle 50 
        |> outlined (solid 0.5) blue,

    -- CAST
    text "C"
        |> sansserif
        |> size 10
        |> filled black
    ]

type Msg = 
    Tick Float GetKeyState |
    UpdateAngle Float

type alias Model = { 
    time  : Float,
    angle : Float,
    quad  : Quadrant }

update msg model = 
    case msg of
        Tick t _ -> 
            ({ model | time = t }, Cmd.none)
        UpdateAngle newAngle -> 
            ({ model | angle = limitAngle newAngle,
                      quad  = updateQuad model.angle }, Cmd.none)

limitAngle angle = 
    if angle > 360 then limitAngle (angle - 360) else angle

updateQuad angle =
    if angle > 0 && angle <= 90 then One
    else if angle > 90 && angle <= 180 then Two
    else if angle > 180 && angle <= 270 then Three
    else Four

init = { 
    time = 0,
    angle = 235,
    quad = Three }

main : EllieAppWithTick () Model Msg
main = ellieAppWithTick Tick { 
    init = \_ -> ( init, Cmd.none ), 
    update = update, 
    view = \model -> { title = "Unit Circle", body = view model },
    subscriptions = \_ -> Sub.none }

view model = collage 192 128 (myShapes model)



