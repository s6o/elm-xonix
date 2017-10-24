module Messages
    exposing
        ( Msg(..)
        )

import Cell exposing (Cell)
import Keys exposing (KeyName, KeyState)
import Time exposing (Time)
import Window exposing (Size)


type Msg
    = NoOp
    | Key KeyName KeyState
    | LevelDown
    | LevelUp
    | PauseResume
    | PlaceBalls (List (Maybe Cell))
    | SystemTick Time
    | WindowResize Size