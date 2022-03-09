module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Consts exposing (..)
import Circle
import Graphing
import Questions


myShapes model = 
    let
        col = getTheme model.col
    in
    [
        Questions.myShapes model.questions
            |> group,
        group (Graphing.myShapes model.graph)
        |> scale 0.6
        |> move (35,20)
        ,
        group (Circle.myShapes model.circle)
        |> scale 0.72
        |> move (-60,20)
        -- ,
        -- -- Buttons to change theme
        -- group [
        --     roundedRect 25 5 2
        --         |> filled col.buttons
        --         |> move (34, 37),
        --     text "Light Theme"
        --         |> size 4
        --         |> centered
        --         |> filled col.words
        --         |> move (34, 36)
        -- ]
        --     |> notifyTap (SetCol Light),
        -- group [
        --     roundedRect 25 5 2
        --         |> filled col.buttons
        --         |> move (61, 37),
        --     text "Dark Theme"
        --         |> size 4
        --         |> centered
        --         |> filled col.words
        --         |> move (61, 36)
        -- ]
        --     |> notifyTap (SetCol Dark)
    ]


init = {
    circle    = Circle.init,
    graph     = Graphing.init,
    questions = Questions.init,
    col       = Light 
    }

main = gameApp Tick {
    model  = init,
    view   = view,
    update = update,
    title  = "Trig Project" 
    }


type alias Model = { 
      circle    : Circle.Model,
      graph     : Graphing.Model,
      questions : Questions.Model,
      col       : Theme
    }

update : Consts.Msg -> Model -> Model
update msg model =
    case msg of
        Tick _ _ -> 
            { model | circle = Circle.update msg model.circle }
        UpdateAngle _ -> 
            { model | circle = Circle.update msg model.circle,
                      graph  = Graphing.update msg model.graph }
        SetCol _ -> 
            { model | circle     = Circle.update msg model.circle,
                      graph      = Graphing.update msg model.graph,
                      questions  = Tuple.first <| Questions.update msg model.questions }
        SetFunc _ _ -> 
            { model | graph = Graphing.update msg model.graph }
        _ ->
            model

view model = collage 192 128 (myShapes model)