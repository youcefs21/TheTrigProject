module Questions exposing (..)

import Random
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Consts exposing (..)
import Html exposing (i)


myShapes model = 
    let 
        col = getTheme model.col
        actualScore (x, y) = x - y
    in [
        showQuestion col model.currentQ (getFour model.currentQ (Random.initialSeed model.seed)) model.seed model.state model.hover (model.time - model.waitTime) (actualScore model.score) model.next model.clap
            |> move (6, 0),
        showScore col model.state model.time model.score model.maxScore,
        if model.state == Waiting then group []
        else if (model.time - model.waitTime > 10) then group [
            square 1000
                |> ghost
                |> notifyEnter (UpdateState model.state)
        ]
        else group []
    ]
    
{-
 - Change weights based on correctness:
 -     If the answer is correct, the probability the question shows up again decrease by easy%.
 -     Otherwise, the probability increase by hard%.
 -}
updateWeights weightedQs (Q q _) correct =
    case weightedQs of
        [] -> []
        ((weight, Q question c)::qs) -> 
            if question == q then 
                let 
                    -- The weight is at least 100% and at most 200%
                    newWeights = if correct then easy else hard |> (*) weight |> max 1 |> min 2
                in (newWeights, Q question c)::qs
            else (weight, Q question c)::(updateWeights qs (Q q (str "0")) correct)

-- Draws the score and controls clickability
showScore col state time score maxScore = 
    let
        displayScore (correct, incorrect) =
            let
                finalScore = correct - incorrect
            in "Score: " ++ String.fromInt finalScore
    in 
        group [ 
            text (displayScore score)
                |> sansserif
                |> size 10
                |> filled (if state == Waiting then col.scoreWait else if state == Incorrect then col.scoreWrong else col.scoreCorrect )
                |> move (-95, 23)
                --|> move (0.5 * paraX (0.1 * time), 0.5 * paraY (0.1 * time))
                |> move (0 + (if state == Incorrect then (0.25 * sin (time * 20)) else 0), 30 + (if state == Correct then jump 0.5 time else 0)),
            if ((Tuple.first score - Tuple.second score) < maxScore) then
                text ("High score: " ++ String.fromInt maxScore)
                    |> sansserif
                    |> size 4
                    |> filled col.scoreWait
                    |> move (-95, 48)
                    --|> move (0.5 * paraX (0.1 * time), 0.5 * paraY (0.1 * time))
            else
                group []
            ]
            |> move (5, -2)
    
-- Draws the question
showQuestion col (Q question correct) incorrects seed state hover time score next clap = group [
    rts question col.question False
        |> move (-84,-44),
    text "Q."
        |> customFont fonts.sansserif
        |> size 8.5
        |> filled col.question
        |> move (-95,-45),
    drawBubbles col state correct (Tuple.first <| Random.step (shuffle (correct::incorrects)) <| Random.initialSeed seed) 0 hover,
    if state == Waiting then group []
    else 
        group [
            -- Implement next button
            rect 192 40
                |> ghost
                |> move (-3, -50),
            if score /= 0 && ((state == Correct && score > 0) || (state == Incorrect && score < 0)) && modBy 5 score == 0 then
                group [
                    claps time col clap
                        |> group,
                    claps time col clap
                        |> group
                        |> mirrorX
                ]
            else
                group [],
            group [
                roundedRect 40 18 5
                    |> filled (if next then col.optionHover else col.optionFade)
                    |> makeTransparent 0.8,
                text "Next"
                    |> centered
                    |> customFont fonts.sansserif
                    |> filled (if next then col.optionTextH else col.optionText)
                    |> move (0, -4)
                ]
                |> scale 0.5
                |> move (73, -43)
                |> notifyEnter (HoverNext True)
                |> notifyLeave (HoverNext False)
                |> notifyTap (UpdateState state)
        ]
    ]

-- Claps
hi = 75
claps time col clap =
    List.range 1 hi |> List.map (
        \i ->
            let
                x = toFloat <| Tuple.first <| Random.step (Random.int 20 50) (Random.initialSeed i)
                y = toFloat <| Tuple.first <| Random.step (Random.int 50 100) (Random.initialSeed (round x))
                y2 = toFloat <| Tuple.first <| Random.step (Random.int 190 220) (Random.initialSeed (round x))
            in
                text clap
                    |> centered
                    |> filled col.words
                    |> rotate (degrees 15)
                    |> mirrorX
                    |> scale (max 1 (jump 1.05 (y2 * time)))
                    |> move (-120 + 2 * x * time, -y * time^2 + y2 * time - 70 - y)
        )

-- Draws the options
drawBubbles col state correct answers c hover =
    case answers of 
        [] -> group []
        (x::xs) -> group [
            let
                waitingOrIncorrect = state == Waiting || correct /= x && x /= hover
                hovered = state == Waiting && x == hover
            in
                group [ 
                    roundedRect 30 13 3
                        |> filled (
                            if state == Waiting then
                                if x == hover then
                                    col.optionHover
                                else
                                    col.optionFade
                            else if x == hover then
                                if state == Correct then
                                    col.optionCorrect
                                else
                                    col.optionWrong
                            else 
                                if x == correct then
                                    col.optionCorrect
                                else 
                                    col.optionFade)
                        |> move (-40, 0)
                        |> makeTransparent 0.8,
                    rts x (if hovered then col.optionTextH else col.optionText) hovered
                        |> move (-40, -2) 
                    ]
                    |> move(-40 + 37 * c, -55)
                    |> notifyEnter (Hover x True)
                    |> (if state == Waiting then notifyLeave (Hover x False) else identity)
                    |> notifyTap (Select x),
            drawBubbles col state correct xs (c + 1) hover
            ]


type alias Model = { 
    time       : Float, 
    waitTime   : Float,
    currentQ   : Question, 
    weightedQs : List (Float, Question),
    state      : State, 
    hover      : Rad,
    score      : (Int, Int), 
    maxScore   : Int,
    seed       : Int, 
    next       : Bool,
    clap       : String,
    radians    : Bool,
    col        : Theme
    }

init : Model
init = { 
    time       = 0, 
    waitTime   = 0,
    currentQ   = Q (str "0") (str "0"), 
    weightedQs = List.indexedMap (\_ q -> (1.0, q) ) rawQs, 
    state      = Waiting, 
    hover      = C "",
    score      = (0, 0), 
    maxScore   = 0,
    seed       = 0, 
    next       = False,
    clap       = "👏",
    radians    = True,
    col        = Light
    }

update : Consts.Msg -> Model -> ( Model, Cmd Consts.Msg )
update msg model = 
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )
        Choice q ->
            ( { model | currentQ = q }, Cmd.none )
        NewSeed i ->
            ( { model | seed = i }, Cmd.none )
        Select answer ->
            ( if correctAnswer model.currentQ answer 
              then { model | score    = (Tuple.first model.score + 1, Tuple.second model.score),
                             state    = Correct,
                             waitTime = model.time,
                             maxScore = if (Tuple.first model.score - Tuple.second model.score + 1) > model.maxScore then (Tuple.first model.score - Tuple.second model.score + 1) else model.maxScore,
                             clap     = if (Tuple.first model.score - Tuple.second model.score + 1) < 0 then "🤨" else "👏" }
              else { model | score    = (Tuple.first model.score, Tuple.second model.score + 1),
                             state    = Incorrect,
                             waitTime = model.time,
                             maxScore = if (Tuple.first model.score - Tuple.second model.score - 1) > model.maxScore then (Tuple.first model.score - Tuple.second model.score - 1) else model.maxScore,
                             clap     = if (Tuple.first model.score - Tuple.second model.score - 1) < 0 then "🤨" else "👏"  },
              Cmd.none )
        UpdateState currentState ->
            ( { model | state      = Waiting,
                        weightedQs = updateWeights model.weightedQs model.currentQ (currentState == Correct),
                        seed       = Tuple.first <| Random.step anyInt (Random.initialSeed model.seed),
                        hover      = C "",
                        next       = False }, 
              genQ model.weightedQs )
        Hover option h ->
            ( { model | hover = if h then option else (C "") }, Cmd.none )
        HoverNext h ->
            ( { model | next = h }, Cmd.none )
        SetCol t ->
            ( { model | col = t }, Cmd.none )
        ToggleRad ->
            ( { model | radians = not model.radians }, Cmd.none )
        _ -> (model, Cmd.none)

view : Model -> Collage Consts.Msg
view model = collage 192 128 <|
    List.concat <| [
        myShapes model
    ]
    
main : EllieAppWithTick () Model Consts.Msg
main =
    ellieAppWithTick Tick
        { init = \ _ -> ( init, Cmd.batch [genQ <| List.indexedMap (\_ q -> (1.0, q) ) rawQs, Random.generate NewSeed anyInt] )
        , update = update
        , view = \ model -> { title = "TheTrigProject", body = view model }
        , subscriptions = \_ -> Sub.none
        }