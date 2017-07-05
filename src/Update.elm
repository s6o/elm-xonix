module Update exposing (..)

import Color exposing (Color)
import Dict exposing (Dict)
import Model exposing
  ( Cell
  , CellShape(..)
  , Direction(..)
  , Grid
  , KeyName(..)
  , KeyState(..)
  , Model
  , Msg(..)
  )
import Task
import Tuple


{-|-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  case msg of
    Key keyName keyState ->
      keyUpdate m keyName keyState
    NoOp ->
      ( m
      , Cmd.none
      )
    PlaceBalls (ballPositions, _) ->
      let
        newBalls =
          ballPositions
            |> List.map (\(x, y) -> {color = Color.orange, bx = x, by = y, d = SW})
        subGrid =
          newBalls
            |> List.map (\b -> {color = b.color, cx = b.bx, cy = b.by, shape = Circle})
        newGrid =
          m.grid |> (\g -> {g | cells = placeInGrid g.cells subGrid})
      in
        ( { m
            | balls = newBalls
            , grid = newGrid
          }
        , Cmd.none
        )
    SystemTick t ->
      {m | systemTick = t} |> moveBalls
    WindowResize ws ->
      let
        playerPos = (m.grid.config.gridWidth // 2, m.grid.config.gridHeight - 1)
      in
        ( { m
            | wsize = Just ws
            , playerHead =
              m.playerHead
                |> Maybe.map (\ph -> ph)
                |> Maybe.withDefault playerPos
                |> Just
          }
        , Task.succeed True |> Task.perform (\_ -> Key KeyArrowDown KeyPressed)
        )

{-|-}
keyUpdate : Model -> KeyName -> KeyState -> (Model, Cmd Msg)
keyUpdate m keyName keyState =
  if keyState == KeyNotPressed then
    ( m
    , Cmd.none
    )
  else
    ( movePlayerHead m keyName
    , Cmd.none
    )

{-|-}
moveBalls : Model -> (Model, Cmd Msg)
moveBalls m =
  (m, Cmd.none)

{-|-}
movePlayerHead : Model -> KeyName -> Model
movePlayerHead m keyName =
  m.playerHead
    |> Maybe.map (\playerPos ->
      let
        (px, py) = playerPos
        newPos = newPlayerPos m keyName
      in
        { m
          | playerHead = Just newPos
          , playerTrail =
            if not <| isFrameCell m playerPos then
              Debug.log "trail append" (playerPos :: m.playerTrail)
            else
              m.playerTrail
          , grid = m.grid |> (\g ->
              {g | cells =
                m.grid.cells
                  |> Dict.get (Model.toGridKey playerPos)
                  |> Maybe.map (\c ->
                    m.grid.cells
                      |> Dict.update (Model.toGridKey playerPos)
                          (\_ -> {c | color = oldCellColor m playerPos} |> Just)
                      |> Dict.update (Model.toGridKey newPos)
                          (\_ ->
                            { color = m.grid.config.playerColor
                            , cx = Tuple.first newPos
                            , cy = Tuple.second newPos
                            , shape = Rectangle
                            }
                            |> Just
                          )
                  )
                  |> Maybe.withDefault m.grid.cells
              }
            )
        }

    )
    |> Maybe.withDefault m


oldCellColor : Model -> (Int, Int) -> Color
oldCellColor m pos =
  if List.member pos m.filledArea then
    m.grid.config.fillColor
  else
    m.grid.config.trailColor


isFrameCell : Model -> (Int, Int) -> Bool
isFrameCell m pos =
  List.member pos m.filledArea


isFreeCell : Model -> (Int, Int) -> Bool
isFreeCell m pos =
  m.grid.cells
    |> Dict.get (Model.toGridKey pos)
    |> Maybe.map (\c -> c.color == m.grid.config.freeColor)
    |> Maybe.withDefault False


newPlayerPos : Model -> KeyName -> (Int, Int)
newPlayerPos m keyName =
  let
    (px, py) = m.playerHead |> Maybe.withDefault (0, 0)
  in
    case keyName of
      KeyArrowDown ->
        ( px
        , if py + 1 < m.grid.config.gridHeight then py + 1 else py
        )
      KeyArrowLeft ->
        ( if px - 1 > -1 then px - 1 else px
        , py
        )
      KeyArrowRight ->
        ( if px + 1 < m.grid.config.gridWidth then px + 1 else px
        , py
        )
      KeyArrowUp ->
        ( px
        , if py - 1 > -1 then py - 1 else py
        )

placeInGrid : Dict String Cell -> List Cell -> Dict String Cell
placeInGrid grid subgrid =
  case subgrid of
    [] -> grid
    c::cs -> placeInGrid (Dict.insert (Model.toGridKey (c.cx, c.cy)) c grid) cs