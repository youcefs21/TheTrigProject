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
        rect 192 128
            |> filled col.background,
        group [
            Questions.myShapes model.questions
                |> group,
            group [
                Graphing.myShapes model.graph
                    |> group
                    |> scale 0.6
                    |> clip
                        (rect 130 60 |> ghost)
                    |> move (37, 0),
                Circle.myShapes model.circle
                    |> group
                    |> scale 0.72
                    |> move (-58, 0)
                ]
                |> move (0, 5)
                --|> move (paraX (-0.02 * model.time), 0.5 * paraY (-0.02 * model.time))
            ],

        -- Buttons to change function
        group [
            group [
                roundedRect 15 5 2
                    |> filled col.buttons
                    |> makeTransparent 0.7
                    |> move (30, 30),
                text "Sin"
                    |> customFont fonts.sansserif
                    |> size 4
                    |> centered
                    |> filled col.words
                    |> move (30, 28.5)
            ]
                |> notifyTap (SetFunc 45 Sin),
            group [
            roundedRect 15 5 2
                |> filled col.buttons
                |> makeTransparent 0.7
                |> move (48, 30),
            text "Cos"
                |> customFont fonts.sansserif
                |> size 4
                |> centered
                |> filled col.words
                |> move (48, 28.5)
            ]
                |> notifyTap (SetFunc 45 Cos),
            group [
                roundedRect 15 5 2
                    |> filled col.buttons
                    |> makeTransparent 0.7
                    |> move (66, 30),
                text "Tan"
                    |> customFont fonts.sansserif
                    |> size 4
                    |> centered
                    |> filled col.words
                    |> move (66, 28.5)
            ]
                |> notifyTap (SetFunc 45 Tan),
                
            -- Buttons to change theme
            group [
                roundedRect 25 5 2
                    |> filled col.buttons
                    |> makeTransparent 0.7
                    |> move (35, 37),
                text "Light Mode"
                    |> customFont fonts.sansserif
                    |> size 4
                    |> centered
                    |> filled col.words
                    |> move (35, 35.5)
            ]
                |> notifyTap (SetCol Light),
            group [
                roundedRect 25 5 2
                    |> filled col.buttons
                    |> makeTransparent 0.7
                    |> move (61, 37),
                text "Dark Mode"
                    |> customFont fonts.sansserif
                    |> size 4
                    |> centered
                    |> filled col.words
                    |> move (61, 35.5)
            ]
                |> notifyTap (SetCol Dark),

            -- Buttons to change degrees/radians
            group [
                roundedRect 25 5 2
                    |> filled col.buttons
                    |> makeTransparent 0.7
                    |> move (35, 37),
                text "Degrees"
                    |> customFont fonts.sansserif
                    |> size 4
                    |> centered
                    |> filled col.words
                    |> move (35, 35.5)
            ]
                |> move (0, 7)
                |> notifyTap (ToggleRad False),
            group [
                roundedRect 25 5 2
                    |> filled col.buttons
                    |> makeTransparent 0.7
                    |> move (61, 37),
                text "Radians"
                    |> customFont fonts.sansserif
                    |> size 4
                    |> centered
                    |> filled col.words
                    |> move (61, 35.5)
            ]
                |> move (0, 7)
                |> notifyTap (ToggleRad True)
        ]
            |> move (15, 12)
    ]
    

type alias Model = { 
    circle    : Circle.Model,
    graph     : Graphing.Model,
    questions : Questions.Model,
    col       : Theme,
    radians   : Bool,
    time      : Float
    }

init = {
    circle    = Circle.init,
    graph     = Graphing.init,
    questions = Questions.init,
    col       = Light,
    radians   = True,
    time      = 0
    }

update : Consts.Msg -> Model -> ( Model, Cmd Consts.Msg )
update msg model =
    case msg of
        Tick t _ -> 
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
                (newGraph,  graphCmds)  = Graphing.update msg model.graph 
                (newQs,     qCmds)      = Questions.update msg model.questions
            in 
                ( { model | circle     = newCircle,
                            graph      = newGraph,
                            questions  = newQs,
                            time       = t }, Cmd.batch [circleCmds, graphCmds, qCmds] )
        UpdateAngle _ -> 
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
                (newGraph,  graphCmds)  = Graphing.update msg model.graph 
            in
                ( { model | circle = newCircle,
                            graph  = newGraph}, Cmd.batch [circleCmds, graphCmds] )
        ToggleDrag _ -> 
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
            in
                ( { model | circle = newCircle}, Cmd.batch [circleCmds] )
        SetCol t -> 
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
                (newGraph,  graphCmds)  = Graphing.update msg model.graph 
                (newQs,     qCmds)      = Questions.update msg model.questions
            in 
                ( { model | circle     = newCircle,
                            graph      = newGraph,
                            questions  = newQs,
                            col        = t }, Cmd.batch [circleCmds, graphCmds, qCmds] )
        ToggleRad r -> 
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
                (newGraph,  graphCmds)  = Graphing.update msg model.graph 
                (newQs,     qCmds)      = Questions.update msg model.questions
            in 
                ( { model | circle     = newCircle,
                            graph      = newGraph,
                            questions  = newQs,
                            radians    = r }, Cmd.batch [circleCmds, graphCmds, qCmds] )
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
        Hover _ _ ->
            let
                (newQs, qCmds)      = Questions.update msg model.questions
            in
                ( { model | questions = newQs }, Cmd.batch [qCmds] )

view : Model -> Collage Consts.Msg
view model = collage 192 128 <|
    List.concat <| [
        myShapes model
    ]

main : EllieAppWithTick () Model Consts.Msg
main =
    ellieAppWithTick Tick
        { init = \_ -> ( init, Cmd.batch [genQ <| List.indexedMap (\_ q -> (1.0, q) ) rawQs, Random.generate NewSeed anyInt] )
        , update = update
        , view = \model -> { title = "TheTrigProject", body = view model }
        , subscriptions = \_ -> Sub.none
        }