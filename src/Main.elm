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
                |> move (0.5, 5)
                --|> move (paraX (-0.02 * model.time), 0.5 * paraY (-0.02 * model.time))
            ],

        -- Settings
        if model.settings then 
            group [
                -- Exit out of settings
                group [
                    rect 192 128
                        |> filled col.buttons
                        |> makeTransparent 0.4
                ]
                    |> notifyTap ToggleSettings,
                settingsBox model col
                    |> move (70, 10)
            ]
        else 
            settingsButton col
                |> move (89, 57)
    ]

settingsButton col = 
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
        |> notifyTap ToggleSettings

option txt toggled col =
    group [
        square 3
            |> filled (if toggled then col.optionCorrect else col.optionWrong)
            |> makeTransparent 0.7
            |> addOutline (solid 0.25) col.words
            |> move (-17, 0),
        text txt
            |> customFont fonts.sansserif
            |> size 4
            |> filled col.words
            |> move (-13, -1.4)
    ]
    
button txt toggled col =
    group [
        roundedRect 35 7 1
            |> filled col.buttons
            |> makeTransparent (if toggled then 1 else 0.7),
        text txt
            |> customFont fonts.sansserif
            |> centered
            |> bold
            |> size 4.5
            |> filled col.words
            |> move (0, -1.3)
    ]

settingsBox model col = 
    group [

        -- Settings box
        group [
            -- Background
            roundedRect 45 103 2
                |> filled col.buttons
                |> makeTransparent 0.6
                |> move (0, -1),
            text "Settings"
                |> customFont fonts.sansserif
                |> centered
                |> bold
                |> size 7
                |> filled col.words
                |> move (0, 42),
            rect 35 0.25
                |> filled col.grid
                |> move (0, 39),

            
            -- Main settings for unit circle/graphs
            option "CAST / quadrants" model.cast col
                |> move (0, 35)
                |> notifyTap ToggleCAST,
            option "special angles" model.sAngles col
                |> move (0, 29)
                |> notifyTap ToggleSAngles,
            option "side lengths" model.sLengths col
                |> move (0, 23)
                |> notifyTap ToggleSLengths,
            option "y-line on graph" model.yLine col
                |> move (0, 17)
                |> notifyTap ToggleYLine,

            -- Change between degrees and radians
            rect 35 0.25
                |> filled col.grid
                |> move (0, 12.5),
            group [
                rect 42 8
                    |> ghost
                    |> move (0, 1.5),
                text "degrees"
                    |> customFont fonts.sansserif
                    |> size 5
                    |> (if model.radians then identity else bold)
                    |> filled col.words
                    |> makeTransparent (if model.radians then 0.6 else 1)
                    |> move (-20 - (if model.radians then 0 else 1), 0),
                text " / "
                    |> customFont fonts.sansserif
                    |> centered
                    |> size 5
                    |> filled col.words,
                text "radians"
                    |> customFont fonts.sansserif
                    |> size 5
                    |> (if model.radians then bold else identity)
                    |> filled col.words
                    |> makeTransparent (if model.radians then 1 else 0.6)
                    |> move (1.5, 0)
                ]
                |> makeTransparent 0.9
                |> move (0.25, 6)
                |> notifyTap ToggleRad,

            -- Change between functions
            rect 35 0.25
                |> filled col.grid
                |> move (0, 3),
            text "Functions"
                |> customFont fonts.sansserif
                |> centered
                |> bold
                |> size 6
                |> filled col.words
                |> move (0, -3.5),
            option "sin" model.showSin col
                |> move (0, -8)
                |> notifyTap ToggleSin,
            option "cos" model.showCos col
                |> move (0, -14)
                |> notifyTap ToggleCos,
            option "tan" model.showTan col
                |> move (0, -20)
                |> notifyTap ToggleTan,

            -- Change themes
            rect 35 0.25
                |> filled col.grid
                |> move (0, -24),
            text "Themes"
                |> customFont fonts.sansserif
                |> centered
                |> bold
                |> size 6
                |> filled col.words
                |> move (0, -31),
            button "Light" (model.col == Light) col
                |> move (0, -37)
                |> notifyTap (SetCol Light),
            button "Dark" (model.col == Dark) col
                |> move (0, -45)
                |> notifyTap (SetCol Dark)
        ]
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
    showSin   : Bool,
    showCos   : Bool,
    showTan   : Bool,
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
    showSin   = True,
    showCos   = False,
    showTan   = False,
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
        ToggleRad -> 
            let
                (newCircle, circleCmds) = Circle.update msg model.circle
                (newGraph,  graphCmds)  = Graphing.update msg model.graph 
                (newQs,     qCmds)      = Questions.update msg model.questions
            in 
                ( { model | circle     = newCircle,
                            graph      = newGraph,
                            questions  = newQs,
                            radians    = not model.radians }, Cmd.batch [circleCmds, graphCmds, qCmds] )
        ToggleYLine -> 
            let
                (newGraph, graphCmds)  = Graphing.update msg model.graph 
            in 
                ( { model | graph = newGraph, yLine = not model.yLine }, Cmd.batch [graphCmds] )
        ToggleSin -> 
            let
                (newGraph, graphCmds)  = Graphing.update msg model.graph 
            in 
                ( { model | graph = newGraph, showSin = not model.showSin }, Cmd.batch [graphCmds] )
        ToggleCos -> 
            let
                (newGraph, graphCmds)  = Graphing.update msg model.graph 
            in 
                ( { model | graph = newGraph, showCos = not model.showCos }, Cmd.batch [graphCmds] )
        ToggleTan -> 
            let
                (newGraph, graphCmds)  = Graphing.update msg model.graph 
            in 
                ( { model | graph = newGraph, showTan = not model.showTan }, Cmd.batch [graphCmds] )
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