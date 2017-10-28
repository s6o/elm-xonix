module View
    exposing
        ( view
        )

import Cell exposing (Cell, Shape(..))
import Collage as C exposing (Form)
import Dict
import Element as E
import Grid exposing (Grid)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy2)
import Messages exposing (Msg(..))
import Model exposing (GameState(..), Model)
import Window as W


view : Model -> Html Msg
view model =
    model.wsize
        |> Maybe.map
            (\ws ->
                div
                    [ style
                        [ ( "width", toString ws.width ++ "px" )
                        , ( "height", toString ws.height ++ "px" )
                        , ( "overflow", "hidden" )
                        , ( "position", "relative" )
                        ]
                    ]
                    [ div
                        [ style
                            [ ( "position", "absolute" )
                            , ( "top", "50%" )
                            , ( "left", "50%" )
                            , ( "transform", "translate(-50%, -50%)" )
                            ]
                        ]
                        [ div
                            [ style
                                [ ( "width", "100%" )
                                ]
                            ]
                            [ div
                                [ style
                                    [ ( "width", "15%" )
                                    , ( "float", "left" )
                                    , ( "padding-bottom", "5px" )
                                    ]
                                ]
                                [ "Level: "
                                    ++ toString model.level
                                    |> text
                                ]
                            , div
                                [ style
                                    [ ( "width", "15%" )
                                    , ( "float", "left" )
                                    , ( "padding-bottom", "5px" )
                                    ]
                                ]
                                [ "Lives: "
                                    ++ toString model.lives
                                    |> text
                                ]
                            , div
                                [ style
                                    [ ( "width", "15%" )
                                    , ( "float", "left" )
                                    , ( "padding-bottom", "5px" )
                                    ]
                                ]
                                [ "Score: "
                                    ++ toString model.score
                                    |> text
                                ]
                            , div
                                [ style
                                    [ ( "width", "20%" )
                                    , ( "float", "left" )
                                    , ( "padding-bottom", "5px" )
                                    ]
                                ]
                                [ toString model.levelFill
                                    |> (\v -> "Captured: " ++ v ++ " %")
                                    |> text
                                ]
                            , div
                                [ style
                                    [ ( "width", "35%" )
                                    , ( "float", "left" )
                                    , ( "padding-bottom", "5px" )
                                    , ( "text-align", "right" )
                                    ]
                                ]
                                [ button
                                    [ onClick LevelDown
                                    , style
                                        [ ( "margin-right", "5px" )
                                        , ( "width", "90px" )
                                        ]
                                    ]
                                    [ text "Level Down"
                                    ]
                                , button
                                    [ onClick LevelUp
                                    , style [ ( "width", "90px" ) ]
                                    ]
                                    [ text "Level Up"
                                    ]
                                ]
                            ]
                        , lazy2 render ws model.grid
                        , div
                            [ style
                                [ ( "width", "100%" )
                                , ( "text-align", "right" )
                                , ( "padding-top", "5px" )
                                ]
                            ]
                            [ button
                                [ onClick PauseResume
                                , disabled (model.game == Stopped)
                                , style
                                    [ ( "margin-right", "5px" )
                                    , ( "width", "90px" )
                                    ]
                                ]
                                [ (case model.game of
                                    Paused ->
                                        "Resume"

                                    Playing ->
                                        "Pause"

                                    Stopped ->
                                        "Game Over"
                                  )
                                    |> text
                                ]
                            , button
                                [ onClick NewGame
                                , disabled (model.game /= Stopped)
                                , style
                                    [ ( "width", "90px" )
                                    ]
                                ]
                                [ text "New Game"
                                ]
                            ]
                        ]
                    ]
            )
        |> Maybe.withDefault (text "Waiting for Window size ...")


boardWidth : Grid -> Int
boardWidth g =
    g.size.width * Cell.size


boardHeight : Grid -> Int
boardHeight g =
    g.size.height * Cell.size


render : W.Size -> Grid -> Html Msg
render ws g =
    [ renderGrid g
    ]
        |> C.collage (boardWidth g) (boardHeight g)
        |> E.container (boardWidth g) (boardHeight g) E.middle
        |> E.toHtml


renderGrid : Grid -> Form
renderGrid g =
    g.cells
        |> Dict.values
        |> List.map (Maybe.map (\c -> renderCell g c) >> Maybe.withDefault (C.group []))
        |> C.group


renderCell : Grid -> Cell -> Form
renderCell g c =
    case c.shape of
        Ball direction ->
            let
                ( cx, cy ) =
                    canvasXY g c

                ( dx, dy ) =
                    Cell.dxdy direction g.animationDelta ( cx, cy )
            in
            Cell.size
                // 2
                |> toFloat
                |> C.circle
                |> C.filled c.color
                |> C.move ( dx, dy )

        Border ->
            renderBlock g c

        Player ->
            renderBlock g c

        Trail ->
            renderBlock g c


renderBlock : Grid -> Cell -> Form
renderBlock g c =
    toFloat Cell.size
        |> C.square
        |> C.filled c.color
        |> C.move (canvasXY g c)


canvasXY : Grid -> Cell -> ( Float, Float )
canvasXY g c =
    let
        boardW =
            boardWidth g

        boardH =
            boardHeight g
    in
    ( ((boardW // 2) - boardW)
        + (c.cellX * Cell.size)
        + (Cell.size // 2)
        |> toFloat
    , boardH
        - (boardH // 2)
        - (c.cellY * Cell.size)
        - (Cell.size // 2)
        |> toFloat
    )
