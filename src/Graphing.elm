module Graphing exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import String
import Set exposing (Set)
import String exposing (fromFloat)
import Consts exposing (..)

myShapes model = 
    let
        col = getTheme model.col
        func = getFunc model.func
    in [
        -- Function
        List.map 
            (\i -> 
                let
                    slopeTooBig = abs (((func (i - deltaX)) - (func i)) / deltaX) > 1000
                in 
                    if slopeTooBig then 
                        group []
                    else 
                        let 
                            x1 = (i - deltaX) * scaleX
                            y1 = model.scaleY * func (i - deltaX)
                            x2 = i * scaleX
                            y2 = model.scaleY * func i
                        in
                            (line (x1, y1) (x2, y2)
                                |> outlined (solid 0.5) (getCol model.func col)
                                |> move (-90, 0))) 
            (rangeStep deltaX (2 * pi) deltaX)
            |> group,
        -- Vertical scale
        List.map (\i -> group 
                        [
                            text (String.fromFloat i)
                            |> size 4
                            |> centered
                            |> filled col.grid
                            |> move (-95, i * model.scaleY - 1.3)
                            |> notifyTap (ClickButton (-90 + i * scaleX)),
                            line (-2, 0) (2, 0)
                            |> outlined (solid 0.5) black
                            |> move (-90, i * model.scaleY)
                        ])                      
                        [-1, 0, 1]
            |> group,
        group [
            -- Vertical Grid
            line (-90, -50) (-90, 50)
                |> outlined (solid 0.5) col.grid,
            -- Moving line
            line (model.posX, 0) (model.posX, -model.scaleY * func (model.posX / scaleX + 0.065))
                |> outlined (solid 0.5) col.movingLine,
            -- Horizontal Grid
            line (-93, 0) (100, 0)
                |> outlined (solid 0.5) col.grid,
            -- Buttons to change function
            group [
                roundedRect 15 5 2
                    |> filled col.buttons
                    |> move (30, 30),
                text "Sin"
                    |> size 4
                    |> centered
                    |> filled black
                    |> move (30, 29)
            ]
                |> notifyTap (SetFunc 45 Sin),
            group [
            roundedRect 15 5 2
                |> filled col.buttons
                |> move (48, 30),
            text "Cos"
                |> size 4
                |> centered
                |> filled black
                |> move (48, 29)
            ]
                |> notifyTap (SetFunc 45 Cos),
            group [
                roundedRect 15 5 2
                    |> filled col.buttons
                    |> move (66, 30),
                text "Tan"
                    |> size 4
                    |> centered
                    |> filled black
                    |> move (66, 29)
            ]
                |> notifyTap (SetFunc 8 Tan),
            -- Buttons to change theme
            group [
                roundedRect 25 5 2
                    |> filled col.buttons
                    |> move (34, 37),
                text "Light Theme"
                    |> size 4
                    |> centered
                    |> filled black
                    |> move (34, 36)
            ]
                |> notifyTap (SetCol Light),
            group [
                roundedRect 25 5 2
                    |> filled col.buttons
                    |> move (61, 37),
                text "Dark Theme"
                    |> size 4
                    |> centered
                    |> filled black
                    |> move (61, 36)
            ]
                |> notifyTap (SetCol Dark)
        ],
        -- Buttons to move the line
        List.map 
            (\(deg, rad) -> 
                group [
                    roundedRect 10 5 2
                    |> filled col.buttons
                    |> move (-90 + deg * scaleX, 3)
                    |> notifyTap (ClickButton (-90 + deg * scaleX)),
                    text rad
                    |> size 4
                    |> centered
                    |> filled black
                    |> move (-90 + deg * scaleX, 2)
                    |> notifyTap (ClickButton (-90 + deg * scaleX))
                ])                     
            specialAngles
            |> group
    ]

deltaX = 0.01
scaleX = 28

type alias Model = { 
      time   : Float,
      quad   : Quadrant,
      func   : Func,
      posX   : Float, 
      angle  : Float,
      scaleY : Float,
      col    : Theme
    }

update : Consts.Msg -> Model -> Model
update msg model = 
    case msg of
        Tick t _ -> 
            { model | time = t}
        ClickButton x -> 
            { model | posX = x }
        SetFunc s f -> 
            { model | func = f, scaleY = toFloat s }
        SetCol t -> 
            { model | col = t }
        UpdateAngle newAngle -> 
            let 
                newNewAngle = limitAngle newAngle
            in
                { model | angle = newNewAngle,
                           quad  = updateQuad newNewAngle }

init : Model
init = { 
    time = 0, 
    func = Sin,
    angle = 45,
    quad  = One,
    posX = -90, 
    scaleY = 45, 
    col = Light }

main = 
    gameApp Tick { 
        model = init, 
        view = view, 
        update = update, 
        title = "Game Slot" }

view model = collage 192 128 (myShapes model)

rangeStep : Float -> Float -> Float -> List Float
rangeStep start stop step = 
    List.map (\x -> (toFloat x - start) * step + start) (List.range (round start) (floor (stop / step)))