module Graphing exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import String
import Element.Font exposing (monospace)
import Set exposing (Set)
import String exposing (fromFloat)

myShapes model =
  (rect 250 150 -- Background
      |> filled model.col.background)
  ::
  -- Function
  List.map (\i -> line ((i - deltaX) * scaleX, model.scaleY * model.func (i - deltaX)) (i * scaleX, model.scaleY * model.func i)
                      |> outlined (solid 0.5) model.col.curve
                      |> move (-90, 0)) 
                      (rangeStep deltaX (2 * pi) deltaX)
  ++
  -- Vertical scale
  List.map (\i -> group 
                  [
                    text (String.fromFloat i)
                      |> size 4
                      |> centered
                      |> filled model.col.grid
                      |> move (-95, i * model.scaleY - 1.3)
                      |> notifyTap (ClickButton (-90 + i * scaleX)),
                    line (-2, 0) (2, 0)
                      |> outlined (solid 0.5) black
                      |> move (-90, i * model.scaleY)
                  ])                      
                [-1, 0, 1]
  ++
  [
  -- Vertical Grid
  line (-90, -50) (-90, 50)
    |> outlined (solid 0.5) model.col.grid,
  -- Moving line
  line (model.posX, 0) (model.posX, -model.scaleY * model.func (model.posX / scaleX + 0.065))
    |> outlined (solid 0.5) model.col.movingLine,
  -- Horizontal Grid
  line (-93, 0) (100, 0)
    |> outlined (solid 0.5) model.col.grid,
  -- Buttons to change function
  roundedRect 15 5 2
    |> filled model.col.buttons
    |> move (30, 30)
    |> notifyTap (SetFunc 45 green sin),
  text "Sin"
    |> size 4
    |> centered
    |> filled black
    |> move (30, 29)
    |> notifyTap (SetFunc 45 green sin),
  roundedRect 15 5 2
    |> filled model.col.buttons
    |> move (48, 30)
    |> notifyTap (SetFunc 45 blue cos),
  text "Cos"
    |> size 4
    |> centered
    |> filled black
    |> move (48, 29)
    |> notifyTap (SetFunc 45 blue cos),
  roundedRect 15 5 2
    |> filled model.col.buttons
    |> move (66, 30)
    |> notifyTap (SetFunc 8 red tan),
  text "Tan"
    |> size 4
    |> centered
    |> filled black
    |> move (66, 29)
    |> notifyTap (SetFunc 8 red tan),
  -- Buttons to change theme
  roundedRect 25 5 2
    |> filled model.col.buttons
    |> move (34, 37)
    |> notifyTap (SetCol (lightTheme model.col.curve)),
  text "Light Theme"
    |> size 4
    |> centered
    |> filled black
    |> move (34, 36)
    |> notifyTap (SetCol (lightTheme model.col.curve)),
  roundedRect 25 5 2
    |> filled model.col.buttons
    |> move (61, 37)
    |> notifyTap (SetCol (darkTheme model.col.curve)),
  text "Dark Theme"
    |> size 4
    |> centered
    |> filled black
    |> move (61, 36)
    |> notifyTap (SetCol (darkTheme model.col.curve))
  ]
  ++
  -- Buttons to move the line
  List.map2 (\i txt -> group 
                  [
                    roundedRect 10 5 2
                      |> filled model.col.buttons
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
lightTheme col = {
    curve      = col,
    grid       = black,
    buttons    = yellow,
    background = white,
    movingLine = lightGreen
  }
darkTheme col = {
    curve      = col,
    grid       = white,
    buttons    = yellow,
    background = black,
    movingLine = lightGreen
  }

changeCurveFunc : Model -> Color -> Theme
changeCurveFunc model color = {
    curve      = color,
    grid       = model.col.grid,
    buttons    = model.col.buttons,
    background = model.col.background,
    movingLine = model.col.movingLine
  }

type Msg = Tick Float GetKeyState |
           ClickButton Float | 
           SetFunc Int Color (Float -> Float) | 
           SetCol Theme

type alias Theme = {
    curve      : Color, 
    grid       : Color, 
    buttons    : Color,
    background : Color,
    movingLine : Color
  }

type alias Model = { 
      time   : Float ,
      func   : Float -> Float, 
      posX   : Float, 
      scaleY : Float,
      col    : Theme
    }

update : Msg -> Model -> Model
update msg model = case msg of
                     Tick t _ -> { time = t , func = model.func, posX = model.posX, scaleY = model.scaleY, col = model.col}
                     ClickButton x -> {time = model.time, func = model.func, posX = x, scaleY = model.scaleY, col = model.col}
                     SetFunc s c f -> {time = model.time, func = f, posX = model.posX, scaleY = toFloat s, col = (changeCurveFunc model c)}
                     SetCol t -> {time = model.time, func = model.func, posX = model.posX, scaleY = model.scaleY, col = t}

init : Model
init = { time = 0 , func = sin, posX = -90, scaleY = 45, col = lightTheme green}

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

rangeStep : Float -> Float -> Float -> List Float
rangeStep start stop step = 
  List.map (\x -> (toFloat x - start) * step + start) (List.range (round start) (floor (stop / step)))