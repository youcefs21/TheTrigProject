module Questions exposing (..)

import Random
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Consts exposing (..)

-- Model and initialization
type alias Model = 
    { time : Float
    , currentQ : Question
    , weightedQs : List (Float, Question)
    , state : State
    , score : (Int, Int)
    , seed : Int
    }

init : Model
init = { 
         time = 0
       , currentQ = Q "a" "a" ["a", "b"]
       , weightedQs = List.indexedMap (\_ q -> (1.0, q) ) rawQs
       , state = Waiting
       , score = (0, 0)
       , seed = 1337
       }
         
-- Determines if chosen answer is correct
correctAnswer (Q _ correct _) answer = 
    if answer == correct then True else False
    
{-
 - Change weights based on correctness:
 -     If the answer is correct, make the chance the question shows up again decrease by 25%.
 -     Otherwise, make the chance increase by 125%.
 -}
updateWeights weightedQs (Q q _ _) correct =
    case weightedQs of
        [] -> []
        ((weight, Q question c incs)::qs) -> 
            if question == q then 
                let 
                    newWeights = if correct then 0.25 else 1.25 |> (*) weight |> max 0.1 |> min 3.0
                in (newWeights, Q question c incs)::qs
            else (weight, Q question c incs)::(updateWeights qs (Q q "" []) correct)
                   
view : Model -> Collage Consts.Msg
view model = collage 192 128 <|
    List.concat <| [
        myShapes model
    ]

myShapes model = [
    showScore2 model.state model.time (showScore model.score),
    showQuestion model.currentQ model.seed model.state,
    text (String.fromInt model.seed)
        |> filled black
    ]

showScore2 state time score = group [ 
    text score
        |> sansserif
        |> size 10
        |> filled (if state == Waiting then (rgb 125 245 245) else if state == Incorrect then (rgb 255 87 85) else green)
        |> move (-95, 23)
        |> move (0 + (if state == Incorrect then (1 * sin (time * 20)) else 0), 30 + (if state == Correct then jump 1 time else 0)),
    if state == Waiting then group []
    else group [
        rect 256 128
            |> ghost
            |> makeTransparent 0
            |> notifyTap (UpdateState state)
        ]
    ]
    
-- Draws the question
showQuestion (Q question correct incorrects) seed state = group [
    text question
        |> sansserif
        |> size 8.5
        |> filled (rgb 255 87 85)
        |> move (-84,-45),
    text "Q."
        |> sansserif
        |> size 8.5
        |> filled (rgb 255 87 85)
        |> move (-95,-45),
    drawBubbles state correct (Tuple.first <| Random.step (shuffle (correct::incorrects)) <| Random.initialSeed seed) 0
    ]
jump amt time = amt * abs (sin (time * 10))

-- Shuffles a list (taken from Random.List)
anyInt =
    Random.int Random.minInt Random.maxInt
    
shuffle list =
    Random.map
        (\independentSeed ->
            list
                |> List.foldl
                    (\item ( acc, seed ) ->
                        let
                            ( tag, nextSeed ) =
                                Random.step anyInt seed
                        in
                        ( ( item, tag ) :: acc, nextSeed )
                    )
                    ( [], independentSeed )
                |> Tuple.first
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
        )
        Random.independentSeed

-- Draws out the options
drawBubbles state correct answers c =
    case answers of 
        [] -> group []
        (x::xs) -> group [
            group [ 
                roundedRect 30 10 3
                    |> filled (if state == Waiting || correct /= x then (rgb 126 217 132) else (rgb 94 219 102))|> move (-40, 0),
                text x
                    |> sansserif
                    |> (if state == Waiting || correct /= x then centered else bold)
                    |> centered
                    |> size 5
                    |> filled white
                    |> move (-40, -2) 
                ]
                |> move(-40 + 37 * c, -55)
                |> notifyTap (Select x),
            drawBubbles state correct xs <| c + 1
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