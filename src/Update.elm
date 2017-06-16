module Update exposing (..)

import Model exposing (Model, Msg(..))


{-|-}
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
