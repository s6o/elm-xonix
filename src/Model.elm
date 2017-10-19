module Model
    exposing
        ( KeyName(..)
        , KeyState(..)
        , Model
        , Msg(..)
        )

import Cell exposing (Cell)
import Grid exposing (Grid)
import Time exposing (Time)
import Window as W


type KeyName
    = KeyArrowDown
    | KeyArrowLeft
    | KeyArrowRight
    | KeyArrowUp


type KeyState
    = KeyNotPressed
    | KeyPressed


type alias Model =
    { grid : Grid
    , systemTick : Time
    , wsize : Maybe W.Size
    }


type Msg
    = NoOp
    | Key KeyName KeyState
    | PlaceBalls (List (Maybe Cell))
    | SystemTick Time
    | WindowResize W.Size
