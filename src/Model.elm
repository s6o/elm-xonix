module Model
    exposing
        ( GameState(..)
        , Model
        )

import Cell exposing (Cell)
import Grid exposing (Grid)
import Time exposing (Time)
import Window exposing (Size)


type GameState
    = Paused
    | Playing


type alias Model =
    { game : GameState
    , grid : Grid
    , level : Int
    , lives : Int
    , score : Int
    , systemTick : Time
    , wsize : Maybe Size
    }
