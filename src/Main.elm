module Main exposing (..)

import AnimationFrame as AF
import Collage as C
import Color exposing (Color)
import Dict exposing (Dict)
import Element as E
import Html exposing (Html, text)
import Html.Lazy
import Task
import Time exposing (Time)
import Window as W


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


type alias Cell =
  { color : Color
  , cx : Int
  , cy : Int
  }

type alias Grid =
  { cells : Dict String Cell
  , config : GridConfig
  }

type alias GridConfig =
  { cellSize : Int
  , fillColor : Color
  , gridWidth : Int
  , gridHeight : Int
  }

type alias Model =
  { grid : Grid
  , systemTick : Time
  , wsize : Maybe W.Size
  }

type Msg
  = SystemTick Time
  | WindowResize W.Size


init : (Model, Cmd Msg)
init =
  ( { grid = initGrid
    , systemTick = 0
    , wsize = Nothing
    }
  , Cmd.batch
    [ W.size |> Task.perform WindowResize
    ]
  )

initGridConfig : GridConfig
initGridConfig =
  { cellSize = 10
  , fillColor = Color.lightBlue
  , gridWidth = 64
  , gridHeight = 46
  }

initGrid : Grid
initGrid =
  let
    gc = initGridConfig
    xBorder y =
      List.repeat
        gc.gridWidth
        gc.fillColor
        |> List.indexedMap (\i c ->
          ( toGridCoords i y
          , {color = c, cx = i, cy = y}
          )
        )
    yBorder x =
      List.repeat
        (gc.gridHeight - 2)
        gc.fillColor
        |> List.indexedMap (\i c ->
          ( toGridCoords x (i + 1)
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


toGridCoords : Int -> Int -> String
toGridCoords x y =
  (toString x) ++ "-" ++ (toString y)

subscriptions : Model -> Sub Msg
subscriptions m =
  Sub.batch
    [ W.resizes WindowResize
    , AF.times SystemTick
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  case msg of
    SystemTick t ->
      ( {m | systemTick = t}
      , Cmd.none
      )
    WindowResize ws ->
      ( {m | wsize = Just ws}
      , Cmd.none
      )


view : Model -> Html Msg
view m =
  Html.Lazy.lazy render m

render : Model -> Html Msg
render m =
  let
    cs = m.grid.config.cellSize
    pw = (m.grid.config.gridWidth * m.grid.config.cellSize)
    ph = (m.grid.config.gridHeight * m.grid.config.cellSize)
    toCoords c =
      ( ((pw // 2) - pw) + (c.cx * cs) + (cs // 2) |> toFloat
      , ph - (ph // 2) - (c.cy * cs) - (cs // 2) |> toFloat
      )
  in
    m.wsize
      |> Maybe.map (\ws ->
        Dict.values m.grid.cells
          |> List.map (\c ->
            toFloat cs
              |> C.square
              |> C.filled c.color
              |> C.move (toCoords c)
          )
          |> C.collage pw ph
          |> E.container ws.width ws.height E.middle
          |> E.toHtml
      )
      |> Maybe.withDefault (text "Waiting for Window size ...")