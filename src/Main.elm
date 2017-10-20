module Main exposing (..)

import AnimationFrame as AF
import Html
import Keyboard exposing (KeyCode)
import Model exposing (GameState(..), KeyName(..), KeyState(..), Model, Msg(..))
import Update
import View
import Window as W


{-| Main entry into the Program.
-}
main : Program Never Model Msg
main =
    Html.program
        { init = Update.init
        , subscriptions = subscriptions
        , update = Update.update
        , view = View.view
        }


{-| Subscriptions
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    if model.game == Playing then
        Sub.batch
            [ W.resizes WindowResize
            , AF.times SystemTick
            , Keyboard.downs (key KeyPressed)
            , Keyboard.ups (key KeyNotPressed)
            ]
    else
        Sub.none


key : KeyState -> KeyCode -> Msg
key keyState keyCode =
    case keyCode of
        37 ->
            Key KeyArrowLeft keyState

        39 ->
            Key KeyArrowRight keyState

        40 ->
            Key KeyArrowDown keyState

        38 ->
            Key KeyArrowUp keyState

        _ ->
            NoOp
