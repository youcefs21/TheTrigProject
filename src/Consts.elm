module Consts exposing (..)

import Random
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


type Msg = Tick Float GetKeyState
         | ToggleSettings
         | Tutorial Int Bool
         | HoverMain Int Bool 
         -- Circle Message
         | UpdateAngle Float
         | HoverCircle Int Bool
         | ToggleDrag Bool
         | ToggleCAST
         | ToggleSAngles
         | ToggleSLengths
         | ToggleRad
         -- Graphing Message
         | HoverGraph Int Bool
         | ToggleSin
         | ToggleCos 
         | ToggleTan
         | SetCol Theme
         | ToggleYLine
         -- Questions Message
         | Choice Question 
         | NewSeed Int
         | Select Rad
         | UpdateState State
         | Hover Rad Bool
         | HoverNext Bool


-- Types needed for other files

-- Q (Question) (Right Answer)
type Question = Q Rad Rad

type State = Waiting | Correct | Incorrect

type Theme = Light
           | Dark
type Quadrant = One 
              | Two 
              | Three 
              | Four

type Rad = C String           -- Constant constant
         | R String Bool      -- Root constant negative
         | F (Rad, Rad) Bool  -- Fraction (numerator, denominator) negative
         | O (String, String) -- Operator (operator, radians)

-- RadToDegRad = rtdr
rtdr s = 
    case s of
        "0" ->
            C "0°"
        "π/6" -> 
            C "30°"
        "π/4" -> 
            C "45°"
        "π/3" -> 
            C "60°"
        "π/2" -> 
            C "90°"
        "2π/3" -> 
            C "120°"
        "3π/4" -> 
            C "135°"
        "5π/6" -> 
            C "150°"
        "π" -> 
            C "180°"
        "7π/6" -> 
            C "210°"
        "5π/4" -> 
            C "225°"
        "4π/3" -> 
            C "240°"
        "3π/2" -> 
            C "270°"
        "5π/3" -> 
            C "300°"
        "7π/4" -> 
            C "315°"
        "11π/6" -> 
            C "330°"
        "2π" -> 
            C "360°"
        _ ->
            C "0°"

-- StringToRad = str
str s = 
    case s of
        "0" ->
            C "0"
        "π/6" -> 
            F (C "π", C "6") False
        "π/4" -> 
            F (C "π", C "4") False
        "π/3" -> 
            F (C "π", C "3") False
        "π/2" -> 
            F (C "π", C "2") False
        "2π/3" -> 
            F (C "2π", C "3") False
        "3π/4" -> 
            F (C "3π", C "4") False
        "5π/6" -> 
            F (C "5π", C "6") False
        "π" -> 
            C "π"
        "7π/6" -> 
            F (C "7π", C "6") False
        "5π/4" -> 
            F (C "5π", C "4") False
        "4π/3" -> 
            F (C "4π", C "3") False
        "3π/2" -> 
            F (C "3π", C "2") False
        "5π/3" -> 
            F (C "5π", C "3") False
        "7π/4" -> 
            F (C "7π", C "4") False
        "11π/6" -> 
            F (C "11π", C "6") False
        "2π" -> 
            C "2π"
        "1" ->
            C "1"
        "-1" ->
            C "-1"
        "(√3)/2" ->
            F (R "3" False, C "2") False
        "-(√3)/2" ->
            F (R "3" False, C "2") True
        "1/(√2)" ->
            F (C "1", R "2" False) False
        "-1/(√2)" ->
            F (C "1", R "2" False) True
        "1/2" ->
            F (C "1", C "2") False
        "-1/2" ->
            F (C "1", C "2") True
        "1/(√3)" ->
            F (C "1", R "3" False) False
        "-1/(√3)" ->
            F (C "1", R "3" False) True
        "(√3)" ->
            R "3" False
        "-(√3)" ->
            R "3" True
        _ ->  
            C s

-- Questions
rawQs = sinQs ++ cosQs ++ tanQs

sinQs =
    [ 
    Q (O ("sin", "π/6")) (str "1/2"),
    Q (O ("sin", "π/4")) (str "1/(√2)"),
    Q (O ("sin", "π/3")) (str "(√3)/2"),
    Q (O ("sin", "π/2")) (str "1"),
    Q (O ("sin", "2π/3")) (str "(√3)/2"),
    Q (O ("sin", "3π/4")) (str "1/(√2)"),
    Q (O ("sin", "5π/6")) (str "1/2"),
    Q (O ("sin", "π")) (str "0"),
    Q (O ("sin", "7π/6")) (str "-1/2"),
    Q (O ("sin", "5π/4")) (str "-1/(√2)"),
    Q (O ("sin", "4π/3")) (str "-(√3)/2"),
    Q (O ("sin", "3π/2")) (str "-1"),
    Q (O ("sin", "5π/3")) (str "-(√3)/2"),
    Q (O ("sin", "7π/4")) (str "-1/(√2)"),
    Q (O ("sin", "11π/6")) (str "-1/2"),
    Q (O ("sin", "0")) (str "0"),
    Q (O ("sin", "2π")) (str "0")
    ]

cosQs =
    [
    Q (O ("cos", "π/6")) (str "(√3)/2"),
    Q (O ("cos", "π/4")) (str "1/(√2)"),
    Q (O ("cos", "π/3")) (str "1/2"),
    Q (O ("cos", "π/2")) (str "0"),
    Q (O ("cos", "2π/3")) (str "-1/2"),
    Q (O ("cos", "3π/4")) (str "-1/(√2)"),
    Q (O ("cos", "5π/6")) (str "-(√3)/2"),
    Q (O ("cos", "π")) (str "-1"),
    Q (O ("cos", "7π/6")) (str "-(√3)/2"),
    Q (O ("cos", "5π/4")) (str "-1/(√2)"),
    Q (O ("cos", "4π/3")) (str "-1/2"),
    Q (O ("cos", "3π/2")) (str "0"),
    Q (O ("cos", "5π/3")) (str "1/2"),
    Q (O ("cos", "7π/4")) (str "1/(√2)"),
    Q (O ("cos", "11π/6")) (str "(√3)/2"),
    Q (O ("cos", "0")) (str "1"),
    Q (O ("cos", "2π")) (str "1")
    ]

tanQs = 
    [
    Q (O ("tan", "π/6")) (str "1/(√3)"),
    Q (O ("tan", "π/4")) (str "1"),
    Q (O ("tan", "π/3")) (str "(√3)"),
    Q (O ("tan", "π/2")) (str "DNE"),
    Q (O ("tan", "2π/3")) (str "-(√3)"),
    Q (O ("tan", "3π/4")) (str "-1"),
    Q (O ("tan", "5π/6")) (str "-1/(√3)"),
    Q (O ("tan", "π")) (str "0"),
    Q (O ("tan", "7π/6")) (str "1/(√3)"),
    Q (O ("tan", "5π/4")) (str "1"),
    Q (O ("tan", "4π/3")) (str "(√3)"),
    Q (O ("tan", "3π/2")) (str "DNE"),
    Q (O ("tan", "5π/3")) (str "-(√3)"),
    Q (O ("tan", "7π/4")) (str "-1"),
    Q (O ("tan", "11π/6")) (str "-1/(√3)"),
    Q (O ("tan", "0")) (str "0"),
    Q (O ("tan", "2π")) (str "0")
    ]

-- 14 possible lengths for special angle triangles
lengthBank i =
    case i of
        1 ->
            str "DNE"
        2 ->
            str "0"
        3 ->
            str "1"
        4 ->
            str "-1"
        5 ->
            str "1/(√3)"
        6 ->
            str "-1/(√3)"
        7 ->
            str "(√3)"
        8 ->
            str "-(√3)"
        9 ->
            str "(√3)/2"
        10 ->
            str "-(√3)/2"
        11 ->
            str "1/(√2)"
        12 ->
            str "-1/(√2)"
        13 ->
            str "1/2"
        14 ->
            str "-1/2"
        _ ->
            str "ERROR"

root width = 
    polygon [
        (0, 60), 
        (-37, -35.12), 
        (-56, 5.2896), 
        (-73.35, -6.338), 
        (-71.54, -9.769), 
        (-60.49, -3.192), 
        (-40.571, -50), 
        (-39.3, -50), 
        (2.603, 57), 
        (width - 1, 57), 
        (width, 60), 
        (0, 60)]

-- RadToShape = rts
rts rad c b radians = 
    case rad of
        C s -> 
            group [
                text s
                    |> centered
                    |> (if b then bold else identity)
                    |> customFont fonts.math
                    |> size 6
                    |> filled c
                ]
        R s neg ->
            group [
                if neg then
                    text "-"
                        |> customFont fonts.math
                        |> (if b then bold else identity)
                        |> size 6
                        |> filled c
                        |> move (-6.5, 0.25)
                else
                    group [],
                root ((*) 90 <| toFloat <| String.length s)
                    |> filled c
                    |> (if b then addOutline (solid 5) c else identity)
                    |> scaleX 0.8
                    |> scale 0.045
                    |> move (-1.5, 1.8),
                text s
                    |> centered
                    |> (if b then bold else identity)
                    |> customFont fonts.math
                    |> size 6
                    |> filled c
                ]
        O (o, r) ->
            group [
                text o
                    |> customFont fonts.math
                    |> (if b then bold else identity)
                    |> size 6
                    |> filled c,  
                rts ((if radians then str else rtdr) r) c b radians
                    |> move (15, 0)
                ]
        F (n, d) neg ->
            group [
                if neg then
                    text "-"
                        |> customFont fonts.math
                        |> (if b then bold else identity)
                        |> size 6
                        |> filled c
                        |> move (-6, 0.25)
                else
                    group [],
                rts n c b radians
                    |> move (0, 2.75),
                rect 5 0.25
                    |> filled c
                    |> (if b then addOutline (solid 0.2) c else identity)
                    |> move (0, 2),
                rts d c b radians
                    |> move (0, -3)
                ]

-- easy and hard for updating weights for spaced repetition
easy = 0.75
hard = 1.2

-- Parametric functions for "natural" movement
paraX t = sin (7 * pi * t)
paraY t = cos (5 * pi * t)

-- Determines if chosen answer is correct
correctAnswer (Q _ correct) answer = 
    if answer == correct then True else False
    
-- Generates questions
qGen qss = 
    case qss of
        [] -> Random.constant <| Q (C "ERROR") (C "ERROR")
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

-- Returns 4 other options for the questions
getFour question seed =
    let
        getCorrect (Q _ c) = c
        q = getCorrect question
        (one, seed2) = getOne [q] seed 
        (two, seed3) = getOne [q, one] seed2
        (three, seed4) = getOne [q, one, two] seed3
        (four, _) = getOne [q, one, two, three] seed4
    in
        [one, two, three, four]

getOne qs seed = 
    let 
        (idx, anotherSeed) = Random.step (Random.int 1 14) seed
        seed2 = Tuple.first <| Random.step anyInt anotherSeed
        opt = lengthBank idx
        identical q xss =
            case xss of
                (x::xs) -> 
                    if q == x then
                        True
                    else
                        identical q xs
                _ ->
                    False
    in
        if identical opt qs then
            getOne qs anotherSeed
        else
            (opt, Random.initialSeed seed2)



-- Fonts and Themes

fonts = {
    monospace = "Source Code Pro",
    sansserif = "Helvetica",
    math      = "Cambria Math"
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
    tutBack       = cols.black,
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
    tutWords      = cols.hypesquadYellow,
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
    optionTextH   = cols.white,
    optionText    = cols.white }

darkTheme = {
    background    = cols.darkmodeGrey,
    tutBack       = cols.notQuiteBlack,
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
    tutWords      = cols.idleYellow,
    dots          = cols.lightNitroGrey,
    scoreWait     = cols.white,
    scoreCorrect  = cols.bugHunterGreen,
    scoreWrong    = cols.dndRed,
    question      = cols.white,
    optionWait    = cols.notQuiteBlack,
    optionHover   = cols.lightNitroGrey,
    optionFade    = cols.notQuiteBlack,
    optionCorrect = cols.bugHunterGreen,
    optionWrong   = cols.dndRed,
    optionTextH   = cols.white,
    optionText    = cols.white }