module Main exposing (..)

import AnimationFrame as AF
import Html
import Model exposing (Model, Msg(..))
import Update
import View
import Window as W


{-| Main entry into the Program.
-}
main : Program Never Model Msg
main =
  Html.program
    { init = Model.init
    , subscriptions = subscriptions
    , update = Update.update
    , view = View.view
    }


{-| Subscriptions
-}
subscriptions : Model -> Sub Msg
subscriptions m =
  Sub.batch
    [ W.resizes WindowResize
    , AF.times SystemTick
    ]
