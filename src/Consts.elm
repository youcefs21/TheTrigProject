module Consts exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


type Msg = Tick Float GetKeyState |
            -- Circle Message
            UpdateAngle Float |
            -- Graphing Message
            ClickButton Float | 
            SetFunc Int Color (Float -> Float) | 
            SetCol Theme


type alias Theme = {
    curve      : Color, 
    grid       : Color, 
    buttons    : Color,
    background : Color,
    movingLine : Color
  }