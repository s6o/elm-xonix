module Update
    exposing
        ( update
        )

import Cell
import Grid
import Model
    exposing
        ( KeyName(..)
        , KeyState(..)
        , Model
        , Msg(..)
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

        PlaceBalls ( ballPositions, _ ) ->
            ( { model
                | grid =
                    ballPositions
                        |> List.indexedMap
                            (\i ( x, y ) ->
                                Cell.ball
                                    model.grid.colors.ball
                                    x
                                    y
                                    (Cell.direction <| (i + 1) % 4)
                            )
                        |> Grid.update model.grid
              }
            , Cmd.none
            )

        SystemTick t ->
            ( { model
                | grid =
                    Grid.nextBallPositions model.grid
                        |> Grid.update model.grid
              }
            , Cmd.none
            )

        WindowResize ws ->
            ( { model | wsize = Just ws }
            , Cmd.none
            )


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
