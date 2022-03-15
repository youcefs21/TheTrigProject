module Consts exposing (..)

import Random
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


type Msg = Tick Float GetKeyState
         | ToggleSettings
         -- Circle Message
         | UpdateAngle Float
         | ToggleDrag Bool
         | ToggleCAST
         | ToggleSAngles
         | ToggleSLengths
         | ToggleRad
         -- Graphing Message
         | ToggleSin
         | ToggleCos 
         | ToggleTan
         | SetCol Theme
         | ToggleYLine
         -- Questions Message
         | Choice Question 
         | NewSeed Int
         | Select String
         | UpdateState State
         | Hover String Bool


-- Types needed for other files

-- Q (Question) (Right Answer) ([Incorrect Answers])
type Question = Q String String (List String)

type State = Waiting | Correct | Incorrect

type Theme = Light
           | Dark
type Quadrant = One 
              | Two 
              | Three 
              | Four


-- Questions
rawQs = [ 
    Q "sin(π/4)" "1/(√2)" ["-1/(√2)", "1/2", "-(√3)/2", "1"],
    Q "sin(11π/6)" "-1/2" ["1/2", "-(√3)/2", "0", "(√3)/2"],
    Q "cos(π)" "-1" ["0", "1", "1/2", "-1/2"],
    Q "sin(2π)" "0" ["-1", "1", "-(√3)/2", "(√3)/2"],
    Q "sin(4π/3)" "-(√3)/2" ["-1/2", "-1/(√2)", "1/(√2)", "-1"]
    ]

-- easy and hard for updating weights for spaced repetition
easy = 1.25
hard = 0.25

-- Parametric functions for "natural" movement
paraX t = sin (7 * pi * t)
paraY t = cos (5 * pi * t)

-- Determines if chosen answer is correct
correctAnswer (Q _ correct _) answer = 
    if answer == correct then True else False
    
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
specialAnglesSimple = [
    (0,   "0"),      (45,  "π/4"),  
    (90,  "π/2"),    (135, "3π/4"),
    (180, "π"),      (225, "5π/4"),
    (270, "3π/2"),    (315, "7π/4"),
    (360, "2π")]
adjLengths = [(0, "1"), (30, "(√3)/2"), (45, "1/(√2)"), (60, "1/2"),    (90, "0")]
oppLengths = [(0, "0"), (30, "1/2"),    (45, "1/(√2)"), (60, "(√3)/2"), (90, "1")]

-- Converts from radians to degrees
radToDeg r = 
    let
        absRad = if r < 0 then 2 * pi + r else r
    in 
        round <| 180 * absRad / pi

-- Converts from degrees to radians
degToRad d = (d / 180) * pi

-- Converts from points to degrees
poiToDeg (x, y) = toFloat <| radToDeg <| atan2 y x

-- Radius of unit circle
ur = 50

-- Origin
org = (0, 0)

-- Step for floats instead of ints
rangeStep : Float -> Float -> Float -> List Float
rangeStep start stop step = 
    List.map 
        (\x -> (toFloat x - start) * step + start) 
        (List.range (round start) (floor (stop / step)))

-- "Jump" movement
jump amt time = amt * abs (sin (time * 10))

-- Forces angle to be between 0 and 360
limitAngle angle = 
    if angle > 360 then limitAngle (angle - 360) else angle

-- Returns the quadrant the angle is in
updateQuad angle =
    if angle >= 0  && angle <= 90  then One   else
    if angle > 90  && angle <= 180 then Two   else
    if angle > 180 && angle <= 270 then Three else
                                        Four

-- Returns a float to n decimal points
toN f n = (toFloat <| round (10^n * f)) / 10^n

-- Getter functions

-- Gets the radians from specialAngles given the degrees
getString f i xss = 
    case xss of
        [] -> String.fromFloat <| (abs <| toN (f (degrees i)) 3)
        ((x1, x2)::xs) -> if i == x1 then x2 else getString f i xs

getTheme t = 
    case t of
        Light  -> lightTheme
        Dark   -> darkTheme

-- Fonts and Themes

fonts = {
    monospace = "Consolas",
    sansserif = "Helvetica",
    math      = "Consolas"--"Cambria Math"
    }

cols = {
    white           = rgb 255 255 255,
    darkmodeGrey    = rgb 54  57  63 ,
    developerBlue   = rgb 62  112 221,
    notSoBurple     = rgb 88  102 239,
    onlineGreen     = rgb 61  165 96 ,
    idleYellow      = rgb 249 166 43 ,
    dndRed          = rgb 236 65  69 ,
    braveryPurple   = rgb 155 132 236,
    brillianceCoral = rgb 243 123 104,
    hypesquadYellow = rgb 248 165 50 ,
    balanceTurqoise = rgb 73  221 193,
    bugHunterGreen  = rgb 72  183 132,
    nitroGrey       = rgb 79  93  126,
    lightNitroGrey  = rgb 183 194 206,
    nitroBlue       = rgb 79  93  127,
    blurple         = rgb 114 137 218,
    darkBlurple     = rgb 78  93  148,
    partnerBlue     = rgb 65  135 237,
    boostPink       = rgb 254 115 246,
    streamerPurple  = rgb 88  54  148,
    hyperlinkBlue   = rgb 9   176 242,
    greyple         = rgb 153 170 181,
    darkGrey        = rgb 47  49  54 ,
    notQuiteBlack   = rgb 39  39  42 ,
    black           = rgb 0   0   0  ,
    green           = rgb 87  242 135,
    yellow          = rgb 254 231 92 ,
    fuchsia         = rgb 235 69  158,
    red             = rgb 237 66  69 }

lightTheme = {
    background    = cols.white,
    adj           = cols.blurple,
    opp           = cols.red,
    hyp           = cols.notQuiteBlack,
    tan           = cols.streamerPurple,
    cir           = cols.lightNitroGrey,
    alpha         = cols.darkGrey,
    angle         = cols.darkmodeGrey,
    grid          = cols.darkmodeGrey,
    buttons       = cols.lightNitroGrey,
    words         = cols.notQuiteBlack,
    dots          = cols.darkGrey,
    scoreWait     = cols.notQuiteBlack,
    scoreCorrect  = cols.green,
    scoreWrong    = cols.dndRed,
    question      = cols.notQuiteBlack,
    optionWait    = cols.lightNitroGrey,
    optionHover   = cols.nitroGrey,
    optionFade    = cols.lightNitroGrey,
    optionCorrect = cols.green,
    optionWrong   = cols.dndRed,
    optionTextH   = cols.blurple,
    optionText    = cols.white }

darkTheme = {
    background    = cols.darkmodeGrey,
    adj           = cols.blurple,
    opp           = cols.red,
    hyp           = cols.lightNitroGrey,
    tan           = cols.braveryPurple,
    cir           = cols.white,
    alpha         = cols.lightNitroGrey,
    angle         = cols.white,
    grid          = cols.white,
    buttons       = cols.notQuiteBlack,
    words         = cols.white,
    dots          = cols.lightNitroGrey,
    scoreWait     = cols.white,
    scoreCorrect  = cols.bugHunterGreen,
    scoreWrong    = cols.dndRed,
    question      = cols.white,
    optionWait    = cols.darkGrey,
    optionHover   = cols.notQuiteBlack,
    optionFade    = cols.darkGrey,
    optionCorrect = cols.bugHunterGreen,
    optionWrong   = cols.dndRed,
    optionTextH   = cols.notSoBurple,
    optionText    = cols.white }