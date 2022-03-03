module Circle exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

myShapes model = [
    unitCircle,
    triangle model.angle
    ]

type Quadrant = One | Two | Three | Four

org = (0, 0)
xpos angle = 50 * cos (degrees angle)
ypos angle = 50 * sin (degrees angle)
pos angle = (xpos angle, ypos angle)

triangle angle = group [
    wedge 3 (angle / 360)
        |> outlined (solid 0.5) darkGrey
        |> rotate (degrees (angle / 2)),
    line org (pos angle)
        |> outlined (solid 0.5) red,
    line org (xpos angle, 0)
        |> outlined (dashed 0.5) blue,
    line (xpos angle, 0) (pos angle)
        |> outlined (dashed 0.5) green,
    circle 1
        |> filled red
        |> move (pos angle)
    ]
  
unitCircle = group [
    rect 0.5 110
        |> filled grey,
    rect 110 0.5
        |> filled grey,
    circle 50 
        |> outlined (solid 0.5) blue
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
    angle = 45,
    quad = One }

main : EllieAppWithTick () Model Msg
main = ellieAppWithTick Tick { 
    init = \_ -> ( init, Cmd.none ), 
    update = update, 
    view = \model -> { title = "Unit Circle", body = view model },
    subscriptions = \_ -> Sub.none }

view model = collage 192 128 (myShapes model)



