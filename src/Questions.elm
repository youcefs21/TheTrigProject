module Questions exposing (..)

import Random
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Consts exposing (..)

-- Model and initialization
type alias Model = { 
    time       : Float, 
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
    currentQ   = Q "" "" ["", "", "", ""], 
    weightedQs = List.indexedMap (\_ q -> (1.0, q) ) rawQs, 
    state      = Waiting, 
    score      = (0, 0), 
    seed       = 0, 
    col        = Light
    }
         
-- Determines if chosen answer is correct
correctAnswer (Q _ correct _) answer = 
    if answer == correct then True else False
    
{-
 - Change weights based on correctness:
 -     If the answer is correct, make the chance the question shows up again decrease by hard%.
 -     Otherwise, make the chance increase by easy%.
 -}
easy = 1.25
hard = 0.25
updateWeights weightedQs (Q q _ _) correct =
    case weightedQs of
        [] -> []
        ((weight, Q question c incs)::qs) -> 
            if question == q then 
                let 
                    newWeights = if correct then hard else easy |> (*) weight |> max 0.1 |> min 3.0
                in (newWeights, Q question c incs)::qs
            else (weight, Q question c incs)::(updateWeights qs (Q q "" []) correct)
                   
view : Model -> Collage Consts.Msg
view model = collage 192 128 <|
    List.concat <| [
        myShapes model
    ]

myShapes model = 
    let 
        col = getTheme model.col
    in [
        showQuestion col model.currentQ model.seed model.state,
        showScore2 col model.state model.time (showScore model.score)
    ]

paraY t = sin (2 * t) + sin (60 * t)
paraX t = 2 * (cos t) + sin (2 * t) * cos (60 * t)
showScore2 col state time score = group [ 
    text score
        |> sansserif
        |> size 10
        |> filled (if state == Waiting then col.scoreWait else if state == Incorrect then col.scoreWrong else col.scoreCorrect )
        |> move (-95, 23)
        |> (if state == Waiting then move (paraX (0.05 * time), paraY (0.05 * time) / 2) else identity)
        |> move (0 + (if state == Incorrect then (1 * sin (time * 20)) else 0), 30 + (if state == Correct then jump 1 time else 0)),
    if state == Waiting then group []
    else group [
        rect 192 128
            |> ghost
            |> makeTransparent 0
            |> notifyTap (UpdateState state)
        ]
    ]
    
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
jump amt time = amt * abs (sin (time * 10))

-- Draws out the options
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

-- Draws the score
showScore (correct, incorrect) =
    let
        score = correct - incorrect
    in "Score: " ++ String.fromInt score

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
                             state = Correct }
              else { model | score = (Tuple.first model.score, Tuple.second model.score + 1),
                             state = Incorrect },
              Cmd.none )
        UpdateState currentState ->
            ( { model | state = Waiting,
                        weightedQs = updateWeights model.weightedQs model.currentQ (currentState == Correct),
                        seed = Tuple.first <| Random.step anyInt (Random.initialSeed model.seed) }, 
              genQ model.weightedQs )
        _ -> (model, Cmd.none)
    
main : EllieAppWithTick () Model Consts.Msg
main =
    ellieAppWithTick Tick
        { init = \ _ -> ( init, Cmd.batch [genQ <| List.indexedMap (\_ q -> (1.0, q) ) rawQs, Random.generate NewSeed anyInt] )
        , update = update
        , view = \ model -> { title = "TheTrigProject", body = view model }
        , subscriptions = \_ -> Sub.none
        }