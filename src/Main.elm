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
        -- Background
        rect 192 128
            |> filled col.background,

        -- Main app
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

        if model.tutorial /= 0 then group []
        else group [
            optionButton "?" col (Tutorial 1 True)
                |> move (89 - 12, 56.75)
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
            optionButton "⚙" col ToggleSettings
                |> move (89, 57),

        -- Tutorial
        case model.tutorial of
            0 -> group []
            1 -> 
                group [
                    tutBG (group []) col.tutBack,
                    tutText ["Welcome to our Trig Visualizer!"] col.tutWords True
                ]
                |> notifyTap (Tutorial 2 True)
            2 -> 
                group [
                    tutBG 
                        (roundedRect 73 68 2 |> ghost |> move (-57.5, 5)) 
                        col.tutBack,
                    tutText ["This is the unit circle. You can click the",
                             "buttons on the special angles to move the",
                             "line to that angle and show that point on",
                             "the graph."] col.tutWords False
                        |> move (-18, 10)
                ]
                |> notifyTap (Tutorial 3 True)
            3 ->
                group [
                    tutBG 
                        (roundedRect 73 68 2 |> ghost |> move (-57.5, 5)) 
                        col.tutBack,
                    tutText ["Click and drag inside the circle to go to",
                             "the non-special angles if you want to",
                             "explore. Non-special angles won't be",
                             "quizzed."] col.tutWords False
                        |> move (-18, 10)
                ]
                |> notifyTap (Tutorial 4 True)
            4 -> 
                group [
                    tutBG
                        (roundedRect 118 64 2 |> ghost |> move (36, 5))
                        col.tutBack,
                    tutText ["This is the graph. You can click",
                             "the buttons for special angles to",
                             "move the line there and change",
                             "the angle in the unit circle."] col.tutWords False
                        |> move (-4, -35)
                ]
                |> notifyTap (Tutorial 5 True)
            5 ->
                group [
                    tutBG
                        (roundedRect 185 40 2 |> ghost |> move (0, -55))
                        col.tutBack,
                    tutText ["We have questions for you to test your understanding. Use",
                             "the visualizer to help you in finding the answer."] col.tutWords False
                        |> move (-80, -23)
                ]
                |> notifyTap (Tutorial 6 True)
            6 -> 
                group [
                    tutBG
                        (roundedRect 45 18 2 |> ghost |> move (-72, 54))
                        col.tutBack,
                    tutText ["We also have a score system. Each time you get it right,",
                             "your score increases. If you get it wrong, your score",
                             "decreases. Try to get the highest score!"] col.tutWords False
                        |> move (-90, 37)
                ]
                |> notifyTap (Tutorial 7 True)
            7 ->
                group [
                    tutBG   
                        (roundedRect 10 10 1 |> ghost |> move (89, 57))
                        col.tutBack,
                    tutText ["Finally, we have the settings."] col.tutWords False
                        |> move (4, 50)
                ]
                |> notifyTap (Tutorial 8 True)
            8 -> 
                group [
                    tutBG   
                        (roundedRect 45 103 2 |> ghost |> move (70, 9))
                        col.tutBack,
                    tutText ["In the settings, you can change how",
                             "the unit circle and graph looks.",
                             "","",
                             "You can also toggle between radians",
                             "and degrees, although the questions",
                             "will be in radians.",
                             "", "",
                             "Next, you can choose which functions to",
                             "show on the graph. And, you can",
                             "switch between light and dark modes."] col.tutWords False
                        |> move (-64, 45)
                ]
                |> notifyTap (Tutorial 9 True)
            9 ->
                group [
                    tutBG (group []) col.tutBack,
                    tutText ["And that concludes the tutorial!",
                             "If you want to see it again, click",
                             "the help (?) button."] col.tutWords True
                ]
                |> notifyTap (Tutorial 0 True)
            _ ->
                group [],
         if model.tutorial /= 0 then 
            tutText ["[press anywhere to continue]"] col.tutWords True
                |> makeTransparent 0.7
                |> scale 0.5
                |> move (0, 58)
        else
            group []
    ]

tutBG shape c = 
    rect 192 128 
        |> filled c
        |> makeTransparent 0.85
        |> subtract shape

tutText txts c center = 
    let
        t txt = 
            text txt 
                |> customFont fonts.sansserif
                |> size 6
                |> (if center then centered else identity)
                |> filled c
    in
        List.indexedMap (\idx txt -> t txt |> move (0, -7 * (toFloat idx))) txts
            |> group
    
    

optionButton sym col msg = 
    group [
        roundedRect 10 10 1.5
            |> filled col.buttons
            |> makeTransparent 0.5,
        text sym
            |> size 8
            |> centered
            |> customFont fonts.sansserif
            |> filled col.words
            |> makeTransparent 0.9
            |> move (0, -3)
    ]
        |> notifyTap msg

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
    tutorial  : Int,
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
    tutorial  = 1,
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
        Tutorial i b ->
            ( { model | tutorial = if b then i else model.tutorial + 1,
                        settings = if i == 8 && b then True else False }, Cmd.none )

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