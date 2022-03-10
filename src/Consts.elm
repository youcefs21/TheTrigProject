module Consts exposing (..)

import Random
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


type Msg = Tick Float GetKeyState
         -- Circle Message
         | UpdateAngle Float
         -- Graphing Message
         | SetFunc Int Func
         | SetCol Theme
         -- Questions Message
         | Choice Question 
         | NewSeed Int
         | Select String
         | UpdateState State

-- Q (Question) (Right Answer) ([Incorrect Answers])
type Question = Q String String (List String)

type State = Waiting | Correct | Incorrect

type Func = Sin
          | Cos
          | Tan

type Theme = Light
           | Dark

type Quadrant = One 
              | Two 
              | Three 
              | Four

rawQs = [ 
    Q "sin(π/4)" "1/(√2)" ["-1/(√2)", "1/2", "-(√3)/2", "1"],
    Q "sin(11π/6)" "-1/2" ["1/2", "-(√3)/2", "0", "(√3)/2"],
    Q "cos(π)" "-1" ["0", "1", "1/2", "-1/2"],
    Q "sin(2π)" "0" ["-1", "1", "-(√3)/2", "(√3)/2"],
    Q "sin(4π/3)" "-(√3)/2" ["-1/2", "-1/(√2)", "1/(√2)", "-1"]
    ]
    
-- Generates questions
qGen qss = 
    case qss of
        [] -> Random.constant <| Q "ERROR" "ERROR" ["ERROR"]
        (q::qs) -> Random.weighted q qs
genQ qss = Random.generate Choice <| qGen qss

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

-- Angles are shown as (degrees as Float, radians as String)
-- everything is technically done through degrees
specialAngles = [
    (0,   "0"),      (30,  "π/6"),   (45,  "π/4"),  (60,  "π/3"), 
    (90,  "π/2"),    (120, "2π/3"),  (135, "3π/4"), (150, "5π/6"),
    (180, "π"),      (210, "7π/6"),  (225, "5π/4"), (240, "4π/3"),
    (270, "3π/2"),   (300, "5π/3"),  (315, "7π/4"), (330, "11π/6"),
    (360, "2π")]
adjLengths = [(0, "1"), (30, "(√3)/2"), (45, "1/(√2)"), (60, "1/2"),    (90, "0")]
oppLengths = [(0, "0"), (30, "1/2"),    (45, "1/(√2)"), (60, "(√3)/2"), (90, "1")]

getString i xss = 
    case xss of
        [] -> "ERR"
        ((x1, x2)::xs) -> if i == x1 then x2 else getString i xs

limitAngle angle = 
    if angle > 360 then limitAngle (angle - 360) else angle

updateQuad angle =
    if angle >= 0  && angle <= 90  then One   else
    if angle > 90  && angle <= 180 then Two   else
    if angle > 180 && angle <= 270 then Three else
                                        Four

getTheme t = 
    case t of
        Light -> lightTheme
        Dark  -> darkTheme

getCol f t = 
    case f of
        Sin -> t.opp
        Cos -> t.adj
        Tan -> t.tan

getFunc f = 
    case f of
        Sin -> sin
        Cos -> cos
        Tan -> tan

fonts = {
    monospace = "Consolas",
    sansserif = "Arial",
    math      = "Cambria Math"
    }

lightTheme = {
    adj = blue,
    opp = green,
    hyp = red,
    tan = rgb 0 128 128,
    cir = blue,
    alpha      = darkGrey,
    angle      = red,
    grid       = black,
    buttons    = grey,
    words      = black,
    dots       = black,
    background = white,
    scoreWait  = rgb 125 245 245,
    scoreCorrect  = rgb 87 255 85,
    scoreWrong    = rgb 255 87 85,
    question      = rgb 255 87 85,
    optionWait    = rgb 126 217 132,
    optionFade    = rgb 94 219 102,
    optionText    = white }

    -- {
    -- adj = blue,
    -- opp = green,
    -- hyp = red,
    -- tan = rgb 0 128 128,
    -- cir = lightBlue,
    -- alpha      = darkGrey,
    -- angle      = red,
    -- grid       = black,
    -- buttons    = grey,
    -- words      = black,
    -- dots       = black,
    -- background = white }

darkTheme = lightTheme