module Main exposing (..)

import Random
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
        group [
            Graphing.myShapes model.graph
                |> group
                |> scale 0.6
                |> move (35, 0),
            Circle.myShapes model.circle
                |> group
                |> scale 0.72
                |> move (-60, 0)
            ]
            |> move (0, 10)
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
    seed      = 0,
    col       = Light 
    }

main : EllieAppWithTick () Model Consts.Msg
main =
    ellieAppWithTick Tick
        { init = \_ -> ( init, Cmd.batch [genQ <| List.indexedMap (\_ q -> (1.0, q) ) rawQs, Random.generate NewSeed anyInt] )
        , update = update
        , view = \model -> { title = "TheTrigProject", body = view model }
        , subscriptions = \_ -> Sub.none
        }


type alias Model = { 
    circle    : Circle.Model,
    graph     : Graphing.Model,
    questions : Questions.Model,
    seed      : Int,
    col       : Theme
    }

update : Consts.Msg -> Model -> ( Model, Cmd Consts.Msg )
update msg model =
    case msg of
        Tick _ _ -> 
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
                (newGraph,  graphCmds)  = Graphing.update msg model.graph 
                (newQs,     qCmds)      = Questions.update msg model.questions
            in 
                ( { model | circle     = newCircle,
                            graph      = newGraph,
                            questions  = newQs }, Cmd.batch [circleCmds, graphCmds, qCmds] )
        UpdateAngle _ -> 
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
                (newGraph,  graphCmds)  = Graphing.update msg model.graph 
            in
                ( { model | circle = newCircle,
                            graph  = newGraph}, Cmd.batch [circleCmds, graphCmds] )
        SetCol _ -> 
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
                (newGraph,  graphCmds)  = Graphing.update msg model.graph 
                (newQs,     qCmds)      = Questions.update msg model.questions
            in 
                ( { model | circle     = newCircle,
                            graph      = newGraph,
                            questions  = newQs }, Cmd.batch [circleCmds, graphCmds, qCmds] )
        SetFunc _ _ -> 
            let
                (newGraph, graphCmds)  = Graphing.update msg model.graph 
            in 
                ( { model | graph = newGraph }, Cmd.batch [graphCmds] )
        NewSeed _ ->
            let
                (newQs, qCmds)      = Questions.update msg model.questions
            in
                ( { model | questions = newQs }, Cmd.batch [qCmds] )
        UpdateState _ ->
            let
                (newQs, qCmds)      = Questions.update msg model.questions
            in
                ( { model | questions = newQs }, Cmd.batch [qCmds] )
        Choice _ ->
            let
                (newQs, qCmds)      = Questions.update msg model.questions
            in
                ( { model | questions = newQs }, Cmd.batch [qCmds] )
        Select _ ->
            let
                (newQs, qCmds)      = Questions.update msg model.questions
            in
                ( { model | questions = newQs }, Cmd.batch [qCmds] )

view : Model -> Collage Consts.Msg
view model = collage 192 128 <|
    List.concat <| [
        myShapes model
    ]