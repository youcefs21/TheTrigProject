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
            l = group [
                line (x, y1) (x, y2)
                    |> outlined (dotted 0.5) fcol,
                line (-90, y2) (90, y2)
                    |> outlined (dotted 0.5) fcol,
                circle 1
                    |> filled fcol
                    |> move (x, y2),
                text (String.fromFloat <| toN (getFunc model.func <| (degrees model.angle)) 3)
                    |> customFont fonts.monospace
                    |> size 4
                    |> filled (getCol model.func col)
                    |> move (x + 2, max -40 <| min 75 y2 / 2)
                ]
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

        -- Buttons to move the line
        List.map 
            (\(deg, rad) -> 
                group [
                    roundedRect 12 5 1
                        |> filled col.buttons
                        |> makeTransparent
                            (if deg == model.angle then 0.7 else 0.5)
                        |> move (if deg == 0 then 2 else 0, -0.75),
                    circle 0.5
                        |> filled col.dots
                        |> move (0, 3),
                    text (if model.radians then rad else String.fromFloat deg ++ "°")
                        |> size 4
                        |> customFont fonts.monospace
                        |> (if (deg == model.angle) then bold else identity)
                        |> centered
                        |> filled col.words
                        |> makeTransparent 0.5
                        |> move (if deg == 0 then 2 else 0, -2)
                ]
                    |> move (-90 + (degrees deg) * scaleX, -3)
                    |> notifyTap (UpdateAngle deg))                     
            specialAnglesSimple
            |> group
    ]

deltaX = 0.005
scaleX = 28


type alias Model = { 
    time    : Float,
    quad    : Quadrant,
    func    : Func,
    angle   : Float,
    scaleY  : Float,
    radians : Bool,
    col     : Theme
    }

init : Model
init = { 
    time    = 0, 
    func    = Sin,
    angle   = 45,
    quad    = One,
    scaleY  = 45, 
    radians = True,
    col     = Light }

update : Consts.Msg -> Model -> ( Model, Cmd Consts.Msg )
update msg model = 
    case msg of
        Tick t _ -> 
            ( { model | time = t }, Cmd.none )
        SetFunc s f -> 
            ( { model | func = f, scaleY = toFloat s }, Cmd.none )
        SetCol t -> 
            ( { model | col = t }, Cmd.none )
        UpdateAngle newAngle -> 
            let 
                newNewAngle = limitAngle newAngle
            in
                ( { model | angle = newNewAngle,
                          quad  = updateQuad newNewAngle }, Cmd.none )
        ToggleRad r ->
            ( { model | radians = r }, Cmd.none )
        _ ->
            ( model, Cmd.none )

view : Model -> Collage Consts.Msg
view model = collage 192 128 <|
    List.concat <| [
        myShapes model
    ]

main : EllieAppWithTick () Model Consts.Msg
main =
    ellieAppWithTick Tick
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = \model -> { title = "TheTrigProject", body = view model }
        , subscriptions = \_ -> Sub.none
        }