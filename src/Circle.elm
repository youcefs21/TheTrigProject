module Circle exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Consts exposing (..)


myShapes model = [
    let 
        col = getTheme model.col
    in
        group [
            unitCircle col model.showCast model.showSAngles model.radians model.angle,
            triangle col model.angle model.quad model.showSLengths model.radians,
            let
                f = \(x, y) -> UpdateAngle (poiToDeg (x + 60, y - 7))
            in
                (circle (ur * 1.1)
                    |> ghost
                    |> notifyMouseDown (ToggleDrag True)
                    |> notifyMouseUp (ToggleDrag False)
                    |> notifyMouseDownAt f
                    |> if model.drag then 
                        notifyMouseMoveAt f
                    else
                        identity)
        ]
            |> scale 0.75
        ]

type Drag = Wait
          | Hold
  
unitCircle col showCast showSAngles radians angle = group [
    -- Grid
    group [
        rect 0.5 110
            |> filled col.grid,
        rect 110 0.5
            |> filled col.grid 
        ]
        |> makeTransparent 0.3,

    -- Circle
    circle ur 
        |> outlined (solid 0.5) col.cir
        |> makeTransparent 0.7,

    -- CAST
    if showCast then cast col else group [],

    -- Special Angles
    if showSAngles then angles col radians angle else group []
    ]

-- Draws CAST at the corners of the unit circle
cast col =
    let 
        r = 50
        tText t c = text t |> customFont fonts.sansserif |> centered |> size 10 |> filled c
    in 
    group [
        tText "C" col.adj
            |> move (r, -r),
        tText "A" col.hyp
            |> move (r, r - 5),
        tText "S" col.opp
            |> move (-r, r - 5),
        tText "T" col.tan
            |> move (-r, -r)
    ]

-- Draws the special angles around the unit circle
angles col radians angle = group <| 
    List.map 
        (\(d, r) -> 
            if d == 360 then group [] else
            let 
                str      = if radians then r else (String.fromFloat d ++ "°")
                strlen s = (*) s <| toFloat <| String.length str
                quad     = updateQuad d
                x = xpos (ur + 7.5) d + if quad == One || quad == Four then strlen 1.5 else -(strlen 1.5)
                y = ypos (ur + 7.5) d + if quad == One || quad == Two  then -3 else 0
            in
                group [ 
                    rect (strlen 3) 8
                        |> ghost
                        |> move (x, y + 1),
                    roundedRect 13 5 1
                        |> filled col.buttons
                        |> move (x, y + 1)
                        |> makeTransparent 
                            (if angle == d then 0.7 else 0.5),
                    text str
                        |> customFont fonts.monospace
                        |> size 4
                        |> centered 
                        |> (if angle == d then bold else identity)
                        |> filled col.words 
                        |> makeTransparent 0.5
                        |> move (x, y),
                    circle 0.75
                        |> filled col.dots
                        |> move (pos ur d) 
                ]
                |> notifyTap (UpdateAngle d)) 
        specialAngles

-- Draws the triangle inside the unit circle
triangle col angle quad showSLengths radians =
    let
        alpha = case quad of
                One -> angle
                Two -> 180 - angle
                Three -> angle - 180
                Four -> 360 - angle
    in group [
        -- Alpha angle
        group [
            if angle == 0 then group [] else
            if angle == 90 || angle == 270 then 
                square 3
                    |> outlined (solid 0.3) col.alpha
                    |> move (1.5, 1.5)
                    |> scaleX (if angle == 90 then 1 else -1)
                    |> scaleY (if angle == 90 then 1 else -1)
            else
                wedge 3 (alpha / 360)
                    |> outlined (solid 0.3) col.alpha
                    |> rotate (degrees (alpha / 2))
                    |> scaleX (if quad == Two   || quad == Three then -1 else 1)
                    |> scaleY (if quad == Three || quad == Four  then -1 else 1)
        ]
            |> makeTransparent 0.7,

        -- Full angle
        circle 2 
            |> outlined (solid 0.3) col.angle
            |> makeTransparent 0.6
            |> clip (wedge 5 (angle / 360)
                |> filled col.angle
                |> rotate (degrees (angle / 2))),

        text (if radians then (String.fromFloat <| toN (degToRad angle) 2) ++ " rad" else (String.fromFloat alpha) ++ "°")
            |> size 4
            |> centered
            |> customFont fonts.monospace
            |> filled col.angle
            |> move ((if radians then 26 else 13) * cos (degrees angle), (if radians then 10 else 5) * sin (degrees angle) - 2),

        -- Adjacent
        line org (xpos ur angle, 0)
            |> outlined (dotted 0.5) col.adj,

        -- Opposite
        line (xpos ur angle, 0) (pos ur angle)
            |> outlined (dotted 0.5) col.opp,

        -- Hypothenuse
        line org (pos ur angle)
            |> outlined (solid 0.5) col.hyp,

        -- Side Lengths
        if showSLengths then 
            let
                rawAdj = getString cos alpha adjLengths
                rawOpp = getString sin alpha oppLengths
                adj = (if quad == Two   || quad == Three && rawAdj /= "0" then "-" else "") ++ rawAdj
                opp = (if quad == Three || quad == Four  && rawOpp /= "0" then "-" else "") ++ rawOpp
                tText str c = 
                    text str
                        |> customFont "Consolas"
                        |> centered
                        |> size 4
                        |> filled c
                    
            in
                group [
                    -- Adj
                    tText adj col.adj
                        |> move (xpos (ur / 2) angle, 
                                 if quad == One || quad == Two then -4
                                 else 1.5),
                    -- Opp
                    tText opp col.opp
                        |> move (xpos ur angle +
                                 if quad == One || quad == Four then (toFloat (String.length opp) + 2)
                                 else -(toFloat (String.length opp) + 2), ypos (ur / 2) angle),
                    -- Hyp
                    tText "1" col.hyp
                        |> move (xpos (ur / 2) angle +
                                 if quad == One || quad == Four then -3 else 3, 
                                 ypos (ur / 2) angle + 
                                 if quad == One || quad == Two then 3 else -5)
                ]  
        else group [],

        -- Dot
        circle 1
            |> filled col.hyp
            |> move (pos ur angle)
        ]

-- Returns positions pased on radius and angle
xpos r angle = r * cos (degrees angle)
ypos r angle = r * sin (degrees angle)
pos  r angle = (xpos r angle, ypos r angle)


type alias Model = { 
    time         : Float,
    angle        : Float,
    quad         : Quadrant,
    showCast     : Bool,
    showSAngles  : Bool,
    showSLengths : Bool,
    radians      : Bool,
    drag         : Bool,
    col          : Theme }

init = { 
    time         = 0,
    angle        = 45,
    quad         = One,
    showCast     = True,
    showSAngles  = True,
    showSLengths = True,
    radians      = True,
    drag         = False,
    col          = Light }

update : Consts.Msg -> Model -> ( Model, Cmd Consts.Msg )
update msg model = 
    case msg of
        Tick t _ -> 
            ( { model | time = t }, Cmd.none )
        UpdateAngle newAngle -> 
            let 
                newNewAngle = limitAngle newAngle
            in
                ( { model | angle = newNewAngle,
                            quad  = updateQuad newNewAngle }, Cmd.none )
        SetCol t -> 
            ( { model | col = t }, Cmd.none )
        ToggleDrag b ->
            ( { model | drag = b }, Cmd.none )
        ToggleRad r ->
            ( { model | radians = r }, Cmd.none )
        _ ->
            ( model, Cmd.none )

view : Model -> Collage Consts.Msg
view model = collage 192 128 <|
    List.concat <| [
        myShapes model
    ]

main : EllieAppWithTick () Model Consts.Msg
main =
    ellieAppWithTick Tick
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = \model -> { title = "TheTrigProject", body = view model }
        , subscriptions = \_ -> Sub.none
        }

