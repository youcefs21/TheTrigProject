module Main exposing (..)

import Random
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Consts exposing (..)
import Circle
import Graphing
import Questions

on = 0.8
off = 0.35
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

        -- Settings
        if model.settings then 
            group [
                group [
                    rect 192 128
                        |> ghost
                        |> notifyTap ToggleSettings
                ],
                group [
                    roundedRect 53 35 2
                        |> filled col.optionFade
                        |> makeTransparent 0.2
                        |> move (48, 30),
                    -- Buttons to change unit circle
                    group [
                        roundedRect 25 5 2
                            |> filled col.buttons
                            |> makeTransparent (if model.cast then on else off)
                            |> move (35, 37),
                        text "CAST"
                            |> customFont fonts.sansserif
                            |> size 4
                            |> centered
                            |> filled col.words
                            |> move (35, 35.5)
                    ]
                        |> move (0, -14)
                        |> notifyTap ToggleCAST,
                    group [
                        roundedRect 25 5 2
                            |> filled col.buttons
                            |> makeTransparent (if model.sLengths then on else off)
                            |> move (61, 37),
                        text "Lengths"
                            |> customFont fonts.sansserif
                            |> size 4
                            |> centered
                            |> filled col.words
                            |> move (61, 35.5)
                    ]
                        |> move (0, -14)
                        |> notifyTap ToggleSLengths,
                    group [
                        roundedRect 25 5 2
                            |> filled col.buttons
                            |> makeTransparent (if model.sAngles then on else off)
                            |> move (35, 37),
                        text "Angles"
                            |> customFont fonts.sansserif
                            |> size 4
                            |> centered
                            |> filled col.words
                            |> move (35, 35.5)
                    ]
                        |> move (0, -21)
                        |> notifyTap ToggleSAngles,

                    -- Buttons to change graph
                    group [
                        roundedRect 25 5 2
                            |> filled col.buttons
                            |> makeTransparent (if model.yLine then on else off)
                            |> move (61, 37),
                        text "Y-axis Line"
                            |> customFont fonts.sansserif
                            |> size 4
                            |> centered
                            |> filled col.words
                            |> move (61, 35.5)
                    ]
                        |> move (0, -21)
                        |> notifyTap ToggleYLine,

                    -- Buttons to change function
                    group [
                        roundedRect 15 5 2
                            |> filled col.buttons
                            |> makeTransparent (if model.func == Sin then on else off)
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
                            |> makeTransparent (if model.func == Cos then on else off)
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
                            |> makeTransparent (if model.func == Tan then on else off)
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
                            |> makeTransparent (if model.col == Light then on else off)
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
                            |> makeTransparent (if model.col == Dark then on else off)
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
                            |> makeTransparent (if model.radians then off else on)
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
                            |> makeTransparent (if model.radians then on else off)
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
        else 
            group [
                square 10
                    |> filled col.buttons
                    |> makeTransparent 0,
                text "⚙"
                    |> size 8
                    |> centered
                    |> customFont fonts.sansserif
                    |> filled col.words
                    |> makeTransparent 0.9
                    |> move (0, -3)
                ]
                |> move (89, 57)
                |> notifyTap ToggleSettings
    ]
    

type alias Model = { 
    circle    : Circle.Model,
    graph     : Graphing.Model,
    questions : Questions.Model,
    col       : Theme,
    radians   : Bool,
    settings  : Bool,
    cast      : Bool,
    sLengths  : Bool,
    sAngles   : Bool,
    yLine     : Bool,
    func      : Func,
    time      : Float
    }

init = {
    circle    = Circle.init,
    graph     = Graphing.init,
    questions = Questions.init,
    col       = Light,
    radians   = True,
    settings  = False,
    cast      = True,
    sLengths  = True,
    sAngles   = True,
    yLine     = False,
    func      = Sin,
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
        ToggleSettings ->
            ( { model | settings = not model.settings }, Cmd.none )
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
        ToggleCAST ->
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
            in
                ( { model | circle = newCircle, cast = not model.cast }, Cmd.batch [circleCmds] )
        ToggleSAngles ->
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
            in
                ( { model | circle = newCircle, sAngles = not model.sAngles }, Cmd.batch [circleCmds] )
        ToggleSLengths ->
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
            in
                ( { model | circle = newCircle, sLengths = not model.sLengths }, Cmd.batch [circleCmds] )
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
        ToggleYLine -> 
            let
                (newGraph, graphCmds)  = Graphing.update msg model.graph 
            in 
                ( { model | graph = newGraph, yLine = not model.yLine }, Cmd.batch [graphCmds] )
        SetFunc _ f -> 
            let
                (newGraph, graphCmds)  = Graphing.update msg model.graph 
            in 
                ( { model | graph = newGraph, func = f }, Cmd.batch [graphCmds] )
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