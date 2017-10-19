module Update
    exposing
        ( init
        , update
        )

import Grid
import Model
    exposing
        ( KeyName(..)
        , KeyState(..)
        , Model
        , Msg(..)
        )
import Task exposing (Task)
import Window as W


init : ( Model, Cmd Msg )
init =
    let
        ( g, gc ) =
            Grid.init 1 PlaceBalls
    in
    ( { grid = g
      , systemTick = 0
      , wsize = Nothing
      }
    , Cmd.batch
        [ W.size |> Task.perform WindowResize
        , gc
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        Key keyName keyState ->
            keyUpdate model keyName keyState

        PlaceBalls initialBalls ->
            ( { model
                | grid =
                    Grid.update model.grid initialBalls
                        |> Grid.initAnimation 0
                
              }
            , Cmd.none
            )

        SystemTick t ->
            let
                newModel =
                    if Grid.inAnimation t model.grid then
                        { model
                            | grid = Grid.animate t model.grid
                            , systemTick = t
                        }
                    else
                        { model
                            | grid =
                                Grid.nextBallPositions model.grid
                                    |> Grid.update model.grid
                                    |> Grid.initAnimation t
                            , systemTick = t
                        }
            in
            ( newModel
            , Cmd.none
            )

        WindowResize ws ->
            ( { model | wsize = Just ws }
            , Cmd.none
            )


{-| @private
-}
keyUpdate : Model -> KeyName -> KeyState -> ( Model, Cmd Msg )
keyUpdate model keyName keyState =
    if keyState == KeyNotPressed then
        ( model
        , Cmd.none
        )
    else
        ( model
        , Cmd.none
        )
