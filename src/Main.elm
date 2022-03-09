module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Consts exposing (..)
import Circle
import Graphing


myShapes model = [
        group (Graphing.myShapes model.graph)
        |> scale 0.6
        |> move (35,20)
        ,
        group (Circle.myShapes model.circle)
        |> scale 0.72
        |> move (-60,20)
    ]


init = 
    {
        circle = Circle.init,
        graph  = Graphing.init 
    }

main = gameApp Tick {
    model = init,
    view = view,
    update = update,
    title = "Trig Project" 
    }


type alias Model = { 
      circle   : Circle.Model,
      graph    : Graphing.Model
    }

update : Consts.Msg -> Model -> Model
update msg model =
    case msg of
        Tick _ _ -> 
            { model | circle = Circle.update msg model.circle }
        -- Circle
        UpdateAngle _ -> 
            { model | circle = Circle.update msg model.circle,
                      graph  = Graphing.update msg model.graph }
        -- Graphing
        ClickButton _ -> 
            { model | graph = Graphing.update msg model.graph }
        SetFunc _ _ -> 
            { model | graph = Graphing.update msg model.graph }
        SetCol _ -> 
            { model | graph = Graphing.update msg model.graph }

view model = collage 192 128 (myShapes model)