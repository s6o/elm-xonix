module Model exposing
    ( GameState(..)
    , Model
    , maxLives
    )

import Grid exposing (Grid)
import Window exposing (Size)


type GameState
    = Paused
    | Playing
    | Stopped


type alias Model =
    { game : GameState
    , grid : Grid
    , level : Int
    , levelFill : Int
    , lives : Int
    , score : Int
    , wsize : Maybe Size
    }


maxLives : Int
maxLives =
    3
