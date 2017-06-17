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
      ( {m | wsize = Just ws}
      , Cmd.none
      )

{-|-}
keyUpdate : Model -> KeyName -> KeyState -> (Model, Cmd Msg)
keyUpdate m keyName keyState =
  if keyState == KeyNotPressed then
    (m, Cmd.none)
  else
    ( {m | grid = m.grid |> (\g -> {g | cells = movePlayerHead m keyName})}
    , Cmd.none
    )

{-|-}
movePlayerHead : Model -> KeyName -> Dict String Cell
movePlayerHead m keyName =
  let
    oldPos = Model.toGridCoords m.playerHead
    oldCell = m.grid.cells |> Dict.get oldPos
    toCell c =
      Just <| {c | color = m.grid.config.fillColor}
  in
    case keyName of
      KeyArrowDown -> m.grid.cells
      KeyArrowLeft ->
        m.grid.cells
          |> Dict.get oldPos
          |> Maybe.map (\c ->
            m.grid.cells
              |> Dict.update oldPos (always <| toCell c)
          )
          |> Maybe.withDefault m.grid.cells
      KeyArrowRight -> m.grid.cells
      KeyArrowUp -> m.grid.cells
