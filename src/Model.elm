module Model exposing
  ( Cell
  , Grid
  , GridConfig
  , Model
  , Msg(..)
  , init
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
  , gridWidth : Int
  , gridHeight : Int
  , playerColor :Color
  }

{-| The main application model.
-}
type alias Model =
  { grid : Grid
  , playerHead : (Int, Int)
  , systemTick : Time
  , wsize : Maybe W.Size
  }

{-| Application messages.
-}
type Msg
  = SystemTick Time
  | WindowResize W.Size


{-| Initialize the application model.
-}
init : (Model, Cmd Msg)
init =
  let
    g = initGrid
    playerPos = (g.config.gridWidth // 2, g.config.gridHeight - 1)
    pg =
      let
        (x, y) = playerPos
      in
        { g
          | cells =
            Dict.update
              (toGridCoords playerPos)
              (always <| Just {color = g.config.playerColor, cx = x, cy = y})
              g.cells
        }
  in
    ( { grid = pg
      , playerHead = playerPos
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
  , gridWidth = 64
  , gridHeight = 46
  , playerColor = Color.darkRed
  }

{-| Initialize game `Grid` with things to render.
-}
initGrid : Grid
initGrid =
  let
    gc = initGridConfig
    xBorder y =
      List.repeat
        gc.gridWidth
        gc.fillColor
        |> List.indexedMap (\i c ->
          ( toGridCoords (i, y)
          , {color = c, cx = i, cy = y}
          )
        )
    yBorder x =
      List.repeat
        (gc.gridHeight - 2)
        gc.fillColor
        |> List.indexedMap (\i c ->
          ( toGridCoords (x, (i + 1))
          , {color = c, cx = x, cy = (i + 1)}
          )
        )
  in
    { cells =
      Dict.fromList
        <| List.append (xBorder 0)
        <| List.append (xBorder <| gc.gridHeight - 1)
        <| List.append (yBorder 0) (yBorder <| gc.gridWidth - 1)
    , config = gc
    }

{-| Convert `Grid` coordinates x, y to a String key: "x-y"
-}
toGridCoords : (Int, Int) -> String
toGridCoords (x, y) =
  (toString x) ++ "-" ++ (toString y)
