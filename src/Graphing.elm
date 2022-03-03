module Graphing exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import String
import Element.Font exposing (monospace)
import Set exposing (Set)

myShapes model =
  List.map (\i -> line ((i - deltaX) * scaleX, scaleY * model.func (i - deltaX)) (i * scaleX, scaleY * model.func i)
                      |> outlined (solid 0.5) black
                      |> move (-90, 0)) 
                      (rangeStep deltaX (2 * pi) deltaX)
  ++
  List.map (\i -> group 
                  [
                    text (String.fromFloat i)
                      |> size 4
                      |> centered
                      |> filled black
                      |> move (-95, i * scaleY - 1.3)
                      |> notifyTap (ClickButton (-90 + i * scaleX)),
                    line (-2, 0) (2, 0)
                      |> outlined (solid 0.5) black
                      |> move (-90, i * scaleY)
                  ])                      
                [-1, 0, 1]
  ++
  [
  line (-90, -50) (-90, 50)
    |> outlined (solid 0.5) black,
  line (model.posX, 0) (model.posX, -scaleY * model.func (model.posX / scaleX + 0.065))
    |> outlined (solid 0.5) green,
  line (-93, 0) (100, 0)
    |> outlined (solid 0.5) black,
  roundedRect 15 5 2
    |> filled yellow
    |> move (30, 30)
    |> notifyTap (SetFunc sin),
  text "Sin"
    |> size 4
    |> centered
    |> filled black
    |> move (30, 29)
    |> notifyTap (SetFunc sin),
  roundedRect 15 5 2
    |> filled yellow
    |> move (48, 30)
    |> notifyTap (SetFunc cos),
  text "Cos"
    |> size 4
    |> centered
    |> filled black
    |> move (48, 29)
    |> notifyTap (SetFunc cos),
  roundedRect 15 5 2
    |> filled yellow
    |> move (66, 30)
    |> notifyTap (SetFunc tan),
  text "Tan"
    |> size 4
    |> centered
    |> filled black
    |> move (66, 29)
    |> notifyTap (SetFunc tan)
  ]
  ++
  List.map2 (\i txt -> group 
                  [
                    roundedRect 12 5 2
                      |> filled darkGreen
                      |> move (-90 + i * scaleX, 6)
                      |> notifyTap (ClickButton (-90 + i * scaleX)),
                    text txt
                      |> size 4
                      |> centered
                      |> filled black
                      |> move (-90 + i * scaleX, 5)
                      |> notifyTap (ClickButton (-90 + i * scaleX))
                  ])                      
                      [0, pi/2, pi, 3*pi/2, 2*pi]
                      ["0", "\u{03C0}/2", "\u{03C0}", "3\u{03C0}/2", "2\u{03C0}"]

deltaX = 0.1
scaleX = 28
scaleY = 45

type Msg = Tick Float GetKeyState | ClickButton Float | SetFunc (Float -> Float)

type alias Model = { time : Float , func : Float -> Float, posX : Float}

update msg model = case msg of
                     Tick t _ -> { time = t , func = model.func, posX = model.posX}
                     ClickButton x -> {time = model.time, func = model.func, posX = x}
                     SetFunc f -> {time = model.time, func = f, posX = model.posX}

init = { time = 0 , func = sin, posX = -90}

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

rangeStep : Float -> Float -> Float -> List Float
rangeStep start stop step = 
  List.map (\x -> (toFloat x - start) * step + start) (List.range (round start) (floor (stop / step)))