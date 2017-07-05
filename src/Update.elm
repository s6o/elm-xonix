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
        initDirection idx =
          case idx of
            0 -> NE
            1 -> SE
            2 -> SW
            3 -> NW
            _ -> NW
        newBalls =
          ballPositions
            |> List.indexedMap (\i (x, y) ->
              {color = Color.orange, bx = x, by = y, d = initDirection <| (i + 1) % 4}
            )
        subGrid =
          newBalls
            |> List.map (\b -> {color = b.color, cx = b.bx, cy = b.by, shape = Circle})
        newGrid =
          m.grid |> (\g -> {g | cells = placeInGrid subGrid g.cells})
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
  let
    newBalls =
      m.balls
        |> List.map (\b ->
          let
            (dx, dy, dd) =
              case b.d of
                NE ->
                  if isCornerCell m (b.bx + 1, b.by - 1) b.d then
                    (b.bx - 1, b.by + 1, SW)
                  else
                    if isFrameCell m (b.bx, b.by - 1) then
                      (b.bx + 1, b.by + 1, SE)
                    else if isFrameCell m (b.bx + 1, b.by) then
                      (b.bx - 1, b.by - 1, NW)
                    else
                      (b.bx + 1, b.by - 1, NE)
                NW ->
                  if isCornerCell m (b.bx - 1, b.by - 1) b.d then
                    (b.bx + 1, b.by + 1, SE)
                  else
                    if isFrameCell m (b.bx - 1, b.by) then
                      (b.bx + 1, b.by - 1, NE)
                    else if isFrameCell m (b.bx, b.by - 1) then
                      (b.bx - 1, b.by + 1, SW)
                    else
                      (b.bx - 1, b.by - 1, NW)
                SE ->
                  if isCornerCell m (b.bx + 1, b.by + 1) b.d then
                    (b.bx - 1, b.by - 1, NW)
                  else
                    if isFrameCell m (b.bx + 1, b.by) then
                      (b.bx - 1, b.by + 1, SW)
                    else if isFrameCell m (b.bx, b.by + 1) then
                      (b.bx + 1, b.by - 1, NE)
                    else
                      (b.bx + 1, b.by + 1, SE)
                SW ->
                  if isCornerCell m (b.bx - 1, b.by + 1) b.d then
                    (b.bx + 1, b.by - 1, NE)
                  else
                    if isFrameCell m (b.bx, b.by + 1) then
                      (b.bx - 1, b.by - 1, NW)
                    else if isFrameCell m (b.bx - 1, b.by) then
                      (b.bx + 1, b.by + 1, SE)
                    else
                      (b.bx - 1, b.by + 1, SW)
          in
            {b | bx = dx, by = dy, d = dd}
        )
    oldPositions =
      m.balls
        |> List.map (\b ->
          {color = m.grid.config.freeColor, cx = b.bx, cy = b.by, shape = Rectangle}
        )
    newPositions =
      newBalls
        |> List.map (\b ->
          {color = b.color, cx = b.bx, cy = b.by, shape = Circle}
        )
  in
    ( { m
        | balls = newBalls
        , grid = m.grid |> (\g ->
            {g | cells =
              placeInGrid oldPositions g.cells
                |> placeInGrid newPositions
            }
          )
      }
    , Cmd.none
    )

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
              playerPos :: m.playerTrail
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


isCornerCell : Model -> (Int, Int) -> Direction -> Bool
isCornerCell m (x, y) d =
  (List.member (x, y) m.filledArea)
  &&
  (
    case d of
      NE ->
        (List.member (x - 1, y) m.filledArea) && (List.member (x, y + 1) m.filledArea)
      NW ->
        (List.member (x + 1, y) m.filledArea) && (List.member (x, y + 1) m.filledArea)
      SE ->
        (List.member (x, y - 1) m.filledArea) && (List.member (x - 1, y) m.filledArea)
      SW ->
        (List.member (x, y - 1) m.filledArea) && (List.member (x + 1, y) m.filledArea)
  )

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

placeInGrid : List Cell -> Dict String Cell -> Dict String Cell
placeInGrid subgrid grid =
  case subgrid of
    [] -> grid
    c::cs -> placeInGrid cs (Dict.insert (Model.toGridKey (c.cx, c.cy)) c grid)
