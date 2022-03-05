module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Consts exposing (..)
import Circle 

myShapes model = [
        group (Circle.myShapes model.circle)
        |> scale 0.5
        |> move (-40,0)
    ]


init = { circle = Circle.init
    }

main = gameApp Tick {
    model = init,
    view = view,
    update = update,
    title = "Game Slot" 
    }


type alias Model = { 
      circle   : Circle.Model
    }

update : Consts.Msg -> Model -> Model
update msg model =
    case msg of
        Tick _ _ -> {model |circle = Circle.update msg model.circle}
        -- Circle
        _ -> model

view model = collage 192 128 (myShapes model)