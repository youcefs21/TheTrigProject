module Consts exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


type Msg = Tick Float GetKeyState |
            -- Circle Message
            UpdateAngle Float
