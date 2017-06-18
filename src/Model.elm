module Model exposing
  ( Cell
  , Grid
  , GridConfig
  , KeyName(..)
  , KeyState(..)
  , Model
  , Msg(..)
  , init
  , toGridKey
  )

import Color exposing (Color)
import Dict exposing (Dict)
import Task
import Time exposing (Time)
import Window as W


{-| A cell with background color and `Grid` coordinates.
-}
type alias Cell =
  { color : Color
  , cx : Int
  , cy : Int
  }

{-| The game `Grid`.
-}
type alias Grid =
  { cells : Dict String Cell
  , config : GridConfig
  }

{-| Configuration for a `Grid`.
Grid coordinates start with (0, 0) top/left and go to (gridWidth-1, gridHeight-1)
bottom/right.
-}
type alias GridConfig =
  { cellSize : Int
  , fillColor : Color
  , freeColor : Color
  , gridWidth : Int
  , gridHeight : Int
  , playerColor :Color
  , trailColor : Color
  }

{-|-}
type KeyName
  = KeyArrowDown
  | KeyArrowLeft
  | KeyArrowRight
  | KeyArrowUp

{-|-}
type KeyState
  = KeyNotPressed
  | KeyPressed

{-| The main application model.
-}
type alias Model =
  { grid : Grid
  , playerHead : Maybe (Int, Int)
  , systemTick : Time
  , wsize : Maybe W.Size
  }

{-| Application messages.
-}
type Msg
  = Key KeyName KeyState
  | NoOp
  | SystemTick Time
  | WindowResize W.Size


{-| Initialize the application model.
-}
init : (Model, Cmd Msg)
init =
  ( { grid = initGrid
    , playerHead = Nothing
    , systemTick = 0
    , wsize = Nothing
    }
  , Cmd.batch
    [ W.size |> Task.perform WindowResize
    ]
  )


{-| Initialize game `Grid` configuration.
-}
initGridConfig : GridConfig
initGridConfig =
  { cellSize = 10
  , fillColor = Color.lightBlue
  , freeColor = Color.white
  , gridWidth = 64
  , gridHeight = 46
  , playerColor = Color.darkRed
  , trailColor = Color.darkYellow
  }

{-| Initialize game `Grid` with things to render.
-}
initGrid : Grid
initGrid =
  let
    gc = initGridConfig
    freeSpace list =
      List.repeat (gc.gridHeight - 2) gc.freeColor
        |> List.indexedMap(\y c ->
          List.repeat (gc.gridWidth - 2) c
            |> List.indexedMap (\x c ->
              ( toGridKey (x + 1, y + 1)
              , {color = c, cx = x + 1, cy = y + 1}
              )
            )
        )
        |> List.concat
        |> List.append list
    xBorder y =
      List.repeat
        gc.gridWidth
        gc.fillColor
        |> List.indexedMap (\i c ->
          ( toGridKey (i, y)
          , {color = c, cx = i, cy = y}
          )
        )
    yBorder x =
      List.repeat
        (gc.gridHeight - 2)
        gc.fillColor
        |> List.indexedMap (\i c ->
          ( toGridKey (x, (i + 1))
          , {color = c, cx = x, cy = (i + 1)}
          )
        )
  in
    { cells =
      Dict.fromList
        <| freeSpace
        <| List.append (xBorder 0)
        <| List.append (xBorder <| gc.gridHeight - 1)
        <| List.append (yBorder 0) (yBorder <| gc.gridWidth - 1)
    , config = gc
    }

{-| Convert `Grid` coordinates x, y to a String key: "x-y"
-}
toGridKey : (Int, Int) -> String
toGridKey (x, y) =
  (toString x) ++ "-" ++ (toString y)
