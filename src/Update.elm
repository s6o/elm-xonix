module Update
    exposing
        ( init
        , update
        )

import Grid
import Keys exposing (KeyName, KeyState(..))
import Messages exposing (Msg(..))
import Model
    exposing
        ( GameState(..)
        , Model
        )
import Task exposing (Task)
import Window as W


init : ( Model, Cmd Msg )
init =
    let
        ( g, gc ) =
            Grid.init 1 PlaceBalls
    in
    ( { game = Playing
      , grid = g
      , level = 1
      , lives = 3
      , score = 0
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
            case keyState of
                KeyNotPressed ->
                    ( model
                    , Cmd.none
                    )

                KeyPressed ->
                    ( { model | grid = Grid.movePlayer keyName model.grid }
                    , Cmd.none
                    )

        LevelDown ->
            if model.level - 1 >= 1 then
                setLevel (model.level - 1) model
            else
                ( model
                , Cmd.none
                )

        LevelUp ->
            if model.level + 1 <= 15 then
                setLevel (model.level + 1) model
            else
                ( model
                , Cmd.none
                )

        PauseResume ->
            let
                newState =
                    case model.game of
                        Paused ->
                            Playing

                        Playing ->
                            Paused
            in
            ( { model | game = newState }
            , Cmd.none
            )

        PlaceBalls initialBalls ->
            ( { model
                | grid =
                    Grid.update model.grid initialBalls
                        |> Grid.initAnimation 0
              }
            , Cmd.none
            )

        SystemTick t ->
            if model.game == Playing then
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
            else
                ( model
                , Cmd.none
                )

        WindowResize ws ->
            ( { model | wsize = Just ws }
            , Cmd.none
            )


{-| @private
-}
setLevel : Int -> Model -> ( Model, Cmd Msg )
setLevel level model =
    let
        ( g, gc ) =
            Grid.init level PlaceBalls
    in
    ( { model
        | grid = g
        , level = level
      }
    , gc
    )
