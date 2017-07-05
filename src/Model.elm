module Model exposing
  ( Ball
  , Cell
  , CellShape(..)
  , Direction(..)
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
import Random exposing (Generator, Seed)
import Task exposing (Task)
import Time exposing (Time)
import Window as W


{-|-}
type alias Ball =
  { color : Color
  , bx : Int
  , by : Int
  , d : Direction
  }

{-| A cell with background color and `Grid` coordinates.
-}
type alias Cell =
  { color : Color
  , cx : Int
  , cy : Int
  , shape : CellShape
  }

{-|-}
type CellShape
  = Rectangle
  | Circle

type Direction
  = NE
  | NW
  | SE
  | SW

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
  { balls : List Ball
  , grid : Grid
  , filledArea : List (Int, Int)
  , playerHead : Maybe (Int, Int)
  , playerTrail : List (Int, Int)
  , systemTick : Time
  , wsize : Maybe W.Size
  }

{-| Application messages.
-}
type Msg
  = Key KeyName KeyState
  | NoOp
  | PlaceBalls (List (Int, Int), Seed)
  | SystemTick Time
  | WindowResize W.Size


{-| Initialize the application model.
-}
init : (Model, Cmd Msg)
init =
  let
    g = initGrid
  in
    ( { balls = []
      , grid = g
      , filledArea = g.cells
          |> Dict.values
          |> List.filter (\c -> c.color == g.config.fillColor)
          |> List.map (\c -> (c.cx, c.cy))
      , playerHead = Nothing
      , playerTrail = []
      , systemTick = 0
      , wsize = Nothing
      }
    , Cmd.batch
      [ W.size |> Task.perform WindowResize
      , initBalls 3 g |> Task.perform PlaceBalls
      ]
    )


{-|-}
initBalls : Int -> Grid -> Task x (List (Int, Int), Seed)
initBalls c g =
  Time.now
    |> Task.map (\t ->
      let
        pg = positionGenerator c g
      in
        Basics.round t
          |> Random.initialSeed
          |> Random.step pg
    )


{-|-}
positionGenerator : Int -> Grid -> Generator (List (Int, Int))
positionGenerator c g =
  Random.list c
    <| Random.pair (Random.int 0 (g.config.gridWidth - 2))
      (Random.int 0 (g.config.gridHeight - 2))


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
              , {color = c, cx = x + 1, cy = y + 1, shape = Rectangle}
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
          , {color = c, cx = i, cy = y, shape = Rectangle}
          )
        )
    yBorder x =
      List.repeat
        (gc.gridHeight - 2)
        gc.fillColor
        |> List.indexedMap (\i c ->
          ( toGridKey (x, (i + 1))
          , {color = c, cx = x, cy = (i + 1), shape = Rectangle}
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
