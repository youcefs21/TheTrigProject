module Graphing exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import String
import Consts exposing (..)

myShapes model = 
    let
        col = getTheme model.col
        func = getFunc model.func
        fcol = getCol model.func col
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
                                |> outlined (solid 0.5) fcol
                                |> move (-90, 0))) 
            (rangeStep 0 (2 * pi) deltaX)
            |> group,

        -- Vertical scale
        List.map (\i -> 
            group [
                text (String.fromFloat i)
                    |> customFont fonts.monospace
                    |> size 4
                    |> centered
                    |> filled col.words
                    |> makeTransparent 0.5
                    |> move (-95, i * model.scaleY - 1.3),
                line (-2, 0) (2, 0)
                    |> outlined (solid 0.5) col.grid
                    |> makeTransparent 0.3
                    |> move (-90, i * model.scaleY)
                ])                      
                [-1, 0, 1]
            |> group,

        -- Vertical Grid
        line (-90, -50) (-90, 50)
            |> outlined (solid 0.5) col.grid
            |> makeTransparent 0.3,

        -- Moving line
        let
            x = -90 + (degrees model.angle) * scaleX
            y1 = 0
            y2 = model.scaleY * func (degrees model.angle)
            l = line (x, y1) (x, y2)
                        |> outlined (dotted 0.5) fcol
            dne = 
                text "DNE"
                    |> customFont fonts.monospace
                    |> size 4
                    |> centered
                    |> filled fcol
                    |> move (x, y1 + 1)
            ra = round model.angle
        in
            if ((ra == 90 || ra == 270) && model.func == Tan) then dne
            else l,

        -- Horizontal Grid
        line (-93, 0) (100, 0)
            |> outlined (solid 0.5) col.grid
            |> makeTransparent 0.3,

        -- Buttons to change function
        group [
            roundedRect 15 5 2
                |> filled col.buttons
                |> move (30, 30),
            text "Sin"
                |> size 4
                |> centered
                |> filled col.words
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
            |> filled col.words
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
                |> filled col.words
                |> move (66, 29)
        ]
            |> notifyTap (SetFunc 8 Tan),

        -- Buttons to move the line
        List.map 
            (\(deg, rad) -> 
                group [
                    roundedRect 7 3 1
                        |> filled col.buttons
                        |> makeTransparent
                            (if deg == model.angle then 0.7 else 0.5)
                        |> move (if deg == 0 then 2 else 0, 0),
                    circle 0.5
                        |> filled col.words
                        |> move (0, 3),
                    text rad
                        |> size 2
                        |> customFont fonts.monospace
                        |> (if (deg == model.angle) then bold else identity)
                        |> centered
                        |> filled col.words
                        |> makeTransparent 0.5
                        |> move (if deg == 0 then 2 else 0, -0.75)
                ]
                    |> move (-90 + (degrees deg) * scaleX, -3)
                    |> notifyTap (UpdateAngle deg))                     
            specialAngles
            |> group
    ]

deltaX = 0.005
scaleX = 28

type alias Model = { 
      time   : Float,
      quad   : Quadrant,
      func   : Func,
      angle  : Float,
      scaleY : Float,
      col    : Theme
    }

update : Consts.Msg -> Model -> Model
update msg model = 
    case msg of
        Tick t _ -> 
            { model | time = t}
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
        _ ->
            model

init : Model
init = { 
    time = 0, 
    func = Sin,
    angle = 45,
    quad  = One,
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
    List.map 
        (\x -> (toFloat x - start) * step + start) 
        (List.range (round start) (floor (stop / step)))