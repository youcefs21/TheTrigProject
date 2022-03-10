module Questions exposing (..)

import Random
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Consts exposing (..)


myShapes model = 
    let 
        col = getTheme model.col
    in [
        showQuestion col model.currentQ model.seed model.state,
        showScore col model.state model.time model.score,
        if model.state == Waiting then group []
        else if (model.time - model.waitTime > 5) then group [
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
updateWeights weightedQs (Q q _ _) correct =
    case weightedQs of
        [] -> []
        ((weight, Q question c incs)::qs) -> 
            if question == q then 
                let 
                    -- The probability is at least 10% and at most 300%
                    newWeights = if correct then easy else hard |> (*) weight |> max 0.1 |> min 3.0
                in (newWeights, Q question c incs)::qs
            else (weight, Q question c incs)::(updateWeights qs (Q q "" []) correct)

-- Draws the score and controls clickability
showScore col state time score = 
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
                |> move (paraX (0.05 * time), paraY (0.05 * time) / 2)
                |> move (0 + (if state == Incorrect then (0.5 * sin (time * 20)) else 0), 30 + (if state == Correct then jump 0.5 time else 0)),
            if state == Waiting then group []
            else group [
                -- Implement next button
                rect 192 40
                    |> ghost
                    |> move (-3, -50),
                group [
                    roundedRect 40 18 5
                        |> filled col.buttons
                        |> makeTransparent 0.7,
                    text "Next"
                        |> centered
                        |> customFont fonts.sansserif
                        |> filled col.words
                        |> move (0, -4)
                    ]
                    |> scale 0.5
                    |> move (70, -41)
                    |> notifyTap (UpdateState state)
                ]
            ]
            |> move (3, -3)
    
-- Draws the question
showQuestion col (Q question correct incorrects) seed state = group [
    text question
        |> customFont fonts.math
        |> size 8.5
        |> filled col.question
        |> move (-84,-45),
    text "Q."
        |> customFont fonts.sansserif
        |> size 8.5
        |> filled col.question
        |> move (-95,-45),
    drawBubbles col state correct (Tuple.first <| Random.step (shuffle (correct::incorrects)) <| Random.initialSeed seed) 0
    ]

-- Draws the options
drawBubbles col state correct answers c =
    case answers of 
        [] -> group []
        (x::xs) -> group [
            group [ 
                roundedRect 30 10 3
                    |> filled (if state == Waiting || correct /= x then col.optionWait else col.optionFade)|> move (-40, 0),
                text x
                    |> customFont fonts.math
                    |> (if state == Waiting || correct /= x then centered else bold)
                    |> centered
                    |> size 5
                    |> filled col.optionText
                    |> move (-40, -2) 
                ]
                |> move(-40 + 37 * c, -55)
                |> notifyTap (Select x),
            drawBubbles col state correct xs <| c + 1
            ]


type alias Model = { 
    time       : Float, 
    waitTime   : Float,
    currentQ   : Question, 
    weightedQs : List (Float, Question),
    state      : State, 
    score      : (Int, Int), 
    seed       : Int, 
    col        : Theme
    }

init : Model
init = { 
    time       = 0, 
    waitTime   = 0,
    currentQ   = Q "" "" ["", "", "", ""], 
    weightedQs = List.indexedMap (\_ q -> (1.0, q) ) rawQs, 
    state      = Waiting, 
    score      = (0, 0), 
    seed       = 0, 
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
              then { model | score = (Tuple.first model.score + 1, Tuple.second model.score),
                             state = Correct,
                             waitTime = model.time }
              else { model | score = (Tuple.first model.score, Tuple.second model.score + 1),
                             state = Incorrect,
                             waitTime = model.time },
              Cmd.none )
        UpdateState currentState ->
            ( { model | state = Waiting,
                        weightedQs = updateWeights model.weightedQs model.currentQ (currentState == Correct),
                        seed = Tuple.first <| Random.step anyInt (Random.initialSeed model.seed) }, 
              genQ model.weightedQs )
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