module Model
    exposing
        ( KeyName(..)
        , KeyState(..)
        , Model
        , Msg(..)
        , init
        )

import Grid exposing (Grid)
import Random exposing (Generator, Seed)
import Task exposing (Task)
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
    | PlaceBalls ( List ( Int, Int ), Seed )
    | SystemTick Time
    | WindowResize W.Size


init : ( Model, Cmd Msg )
init =
    let
        ( g, gc ) =
            Grid.init 1 PlaceBalls
    in
        ( { grid = g
          , systemTick = 0
          , wsize = Nothing
          }
        , Cmd.batch
            [ W.size |> Task.perform WindowResize
            , gc
            ]
        )
