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
      , levelFill = 0
      , lives = Model.maxLives
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

        Conquer ->
            ( { model | grid = Grid.conquer model.grid }
            , Cmd.none
            )

        Key keyName keyState ->
            case keyState of
                KeyNotPressed ->
                    ( model
                    , Cmd.none
                    )

                KeyPressed ->
                    let
                        ( nextGrid, nextCmd ) =
                            Grid.movePlayer keyName model.grid
                    in
                    ( { model | grid = nextGrid }
                    , nextCmd
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

        NewGame ->
            let
                ( g, gc ) =
                    Grid.init 1 PlaceBalls
            in
            ( { model | game = Playing, grid = g, level = 1, lives = Model.maxLives }
            , gc
            )

        PauseResume ->
            let
                newState =
                    case model.game of
                        Paused ->
                            Playing

                        Playing ->
                            Paused

                        _ ->
                            model.game
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
                    ( nextBallPos, nextCmd ) =
                        Grid.nextBallPositions model.grid

                    newModel =
                        if Grid.inAnimation t model.grid then
                            { model
                                | grid = Grid.animate t model.grid
                                , systemTick = t
                            }
                        else
                            { model
                                | grid =
                                    nextBallPos
                                        |> Grid.update model.grid
                                        |> Grid.initAnimation t
                                , systemTick = t
                            }
                in
                ( newModel
                , nextCmd
                )
            else
                ( model
                , Cmd.none
                )

        TakeLife ->
            if model.lives - 1 >= 1 then
                ( { model
                    | grid = Grid.clearTrail model.grid
                    , lives = model.lives - 1
                  }
                , Cmd.none
                )
            else
                ( { model
                    | game = Stopped
                    , grid = Grid.clearTrail model.grid
                    , lives = 0
                  }
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
