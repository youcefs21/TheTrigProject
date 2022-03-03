module Graphing exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import String
import Element.Font exposing (monospace)
import Set exposing (Set)
import String exposing (fromFloat)

myShapes model =
  List.map (\i -> line ((i - deltaX) * scaleX, model.scaleY * model.func (i - deltaX)) (i * scaleX, model.scaleY * model.func i)
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
                      |> move (-95, i * model.scaleY - 1.3)
                      |> notifyTap (ClickButton (-90 + i * scaleX)),
                    line (-2, 0) (2, 0)
                      |> outlined (solid 0.5) black
                      |> move (-90, i * model.scaleY)
                  ])                      
                [-1, 0, 1]
  ++
  [
  line (-90, -50) (-90, 50)
    |> outlined (solid 0.5) black,
  line (model.posX, 0) (model.posX, -model.scaleY * model.func (model.posX / scaleX + 0.065))
    |> outlined (solid 0.5) green,
  line (-93, 0) (100, 0)
    |> outlined (solid 0.5) black,
  roundedRect 15 5 2
    |> filled yellow
    |> move (30, 30)
    |> notifyTap (SetFunc 45 sin),
  text "Sin"
    |> size 4
    |> centered
    |> filled black
    |> move (30, 29)
    |> notifyTap (SetFunc 45 sin),
  roundedRect 15 5 2
    |> filled yellow
    |> move (48, 30)
    |> notifyTap (SetFunc 45 cos),
  text "Cos"
    |> size 4
    |> centered
    |> filled black
    |> move (48, 29)
    |> notifyTap (SetFunc 45 cos),
  roundedRect 15 5 2
    |> filled yellow
    |> move (66, 30)
    |> notifyTap (SetFunc 8 tan),
  text "Tan"
    |> size 4
    |> centered
    |> filled black
    |> move (66, 29)
    |> notifyTap (SetFunc 8 tan)
  ]
  ++
  List.map2 (\i txt -> group 
                  [
                    roundedRect 10 5 2
                      |> filled darkGreen
                      |> move (-90 + i * scaleX, 3)
                      |> notifyTap (ClickButton (-90 + i * scaleX)),
                    text txt
                      |> size 4
                      |> centered
                      |> filled black
                      |> move (-90 + i * scaleX, 2)
                      |> notifyTap (ClickButton (-90 + i * scaleX))
                  ])                      
                      [0, pi/2, pi, 3*pi/2, 2*pi]
                      ["0", "\u{03C0}/2", "\u{03C0}", "3\u{03C0}/2", "2\u{03C0}"]

deltaX = 0.01
scaleX = 28

type Msg = Tick Float GetKeyState | ClickButton Float | SetFunc Int (Float -> Float)

type alias Model = { time : Float , func : Float -> Float, posX : Float, scaleY : Int}

update msg model = case msg of
                     Tick t _ -> { time = t , func = model.func, posX = model.posX, scaleY = model.scaleY}
                     ClickButton x -> {time = model.time, func = model.func, posX = x, scaleY = model.scaleY}
                     SetFunc s f -> {time = model.time, func = f, posX = model.posX, scaleY = toFloat s}

init = { time = 0 , func = sin, posX = -90, scaleY = 45}

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

rangeStep : Float -> Float -> Float -> List Float
rangeStep start stop step = 
  List.map (\x -> (toFloat x - start) * step + start) (List.range (round start) (floor (stop / step)))