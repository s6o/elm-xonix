module Update exposing (..)

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
            , grid = m.grid |> (\g -> {g | cells = movePlayerHead m KeyArrowDown})
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
    ( { m
        | grid = m.grid |> (\g -> {g | cells = movePlayerHead m keyName})
        , playerHead = Just <| newPlayerPos m keyName
      }
    , Cmd.none
    )

{-|-}
movePlayerHead : Model -> KeyName -> Dict String Cell
movePlayerHead m keyName =
  m.playerHead
    |> Maybe.map (\playerPos ->
      let
        _ = Debug.log "movePlayerHead" keyName
        (px, py) = playerPos
        newPos = newPlayerPos m keyName
      in
        m.grid.cells
          |> Dict.get (Model.toGridKey playerPos)
          |> Maybe.map (\c ->
            m.grid.cells
              |> Dict.update (Model.toGridKey playerPos)
                  (\_ -> Debug.log "restore old cell" {c | color = m.grid.config.fillColor} |> Just)
              |> Dict.update (Model.toGridKey newPos)
                  (\_ ->
                    Debug.log "new playerPos 1"
                    { color = m.grid.config.playerColor
                    , cx = Tuple.first newPos
                    , cy = Tuple.second newPos
                    }
                    |> Just
                  )
          )
          |> Maybe.withDefault m.grid.cells
    )
    |> Maybe.withDefault m.grid.cells


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
