module Model
    exposing
        ( GameState(..)
        , KeyName(..)
        , KeyState(..)
        , Model
        , Msg(..)
        )

import Cell exposing (Cell)
import Grid exposing (Grid)
import Time exposing (Time)
import Window as W


type GameState
    = Paused
    | Playing


type KeyName
    = KeyArrowDown
    | KeyArrowLeft
    | KeyArrowRight
    | KeyArrowUp


type KeyState
    = KeyNotPressed
    | KeyPressed


type alias Model =
    { game : GameState
    , grid : Grid
    , level : Int
    , lives : Int
    , score : Int
    , systemTick : Time
    , wsize : Maybe W.Size
    }


type Msg
    = NoOp
    | Key KeyName KeyState
    | LevelDown
    | LevelUp
    | PauseResume
    | PlaceBalls (List (Maybe Cell))
    | SystemTick Time
    | WindowResize W.Size
