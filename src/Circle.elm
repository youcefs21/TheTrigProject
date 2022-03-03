module Circle exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

myShapes model =
  [
    unitCircle,
    wedge 3 (d / 360)
      |> outlined (solid 0.5) darkGrey
      |> rotate (degrees (d / 2)),
    line org pos
      |> outlined (solid 0.5) red,
    line org (xpos, 0)
      |> outlined (dashed 0.5) blue,
    line (xpos, 0) pos
      |> outlined (dashed 0.5) green,
    circle 1
      |> filled red
      |> move pos
  ]

org = (0, 0)
xpos = 50 * cos (degrees d)
ypos = 50 * sin (degrees d)
pos = (xpos, ypos)
d = 45
  
unitCircle = group [
    rect 0.5 110
      |> filled grey,
    rect 110 0.5
      |> filled grey,
    circle 50 
      |> outlined (solid 0.5) blue
  ]

type Msg = Tick Float GetKeyState

type alias Model = { time : Float }

update msg model = case msg of
                     Tick t _ -> { time = t }

init = { time = 0 }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)



