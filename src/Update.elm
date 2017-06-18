module Update exposing (..)

import Color exposing (Color)
import Dict exposing (Dict)
import Model exposing
  ( Cell
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
    SystemTick t ->
      ( {m | systemTick = t}
      , Cmd.none
      )
    WindowResize ws ->
      let
        playerPos = (m.grid.config.gridWidth // 2, m.grid.config.gridHeight - 1)
      in
        ( { m
            | wsize = Just ws
            , playerHead = Just playerPos
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
