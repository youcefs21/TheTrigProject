module Graphing exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import String

myShapes model =
  List.map2 (\i txt -> text txt
                      |> size 4
                      |> centered
                      |> filled black
                      |> move (-90 + i * scaleX, 5))
                      [0, pi/2, pi, 3*pi/2, 2*pi] 
                      ["0", "\u{03C0}/2", "\u{03C0}", "3\u{03C0}/2", "2\u{03C0}"]
  ++
  List.map (\i -> line ((i - deltaX) * scaleX, scaleY * sin (i - deltaX)) (i * scaleX, scaleY * sin i)
                      |> outlined (solid 0.5) black
                      |> move (-90, 0)) 
                      (rangeStep deltaX (2 * pi) deltaX)
  ++
  [
  line (-90, -50) (-90, 50)
    |> outlined (solid 0.5) black,
  line (-100, 0) (100, 0)
    |> outlined (solid 0.5) black
  ]

deltaX = 0.1
scaleX = 28
scaleY = 45

type Func = Sin | Cos | Tan

type Msg = Tick Float GetKeyState

type alias Model = { time : Float , func : Func}

update msg model = case msg of
                     Tick t _ -> { time = t }

init = { time = 0 }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

rangeStep : Float -> Float -> Float -> List Float
rangeStep start stop step = 
  List.map (\x -> (toFloat x - start) * step + start) (List.range (round start) (floor (stop / step)))