module Cell
    exposing
        ( Cell
        , Direction(..)
        , Shape(..)
        , ball
        , border
        , direction
        , equal
        , isSpace
        , player
        , space
        , trail
        , size
        )

import Color exposing (Color)


type alias Cell =
    { cellX : Int
    , cellY : Int
    , color : Color
    , shape : Shape
    }


type Direction
    = NE
    | NW
    | SE
    | SW


type Shape
    = Ball Direction
    | Border
    | Player
    | Space
    | Trail


ball : Color -> Int -> Int -> Direction -> Cell
ball c x y d =
    Cell x y c (Ball d)


border : Color -> Int -> Int -> Cell
border c x y =
    Cell x y c Border


direction : Int -> Direction
direction idx =
    case idx of
        0 ->
            NE

        1 ->
            SE

        2 ->
            SW

        3 ->
            NW

        _ ->
            NW


equal : Shape -> Shape -> Bool
equal s1 s2 =
    case ( s1, s2 ) of
        ( Ball _, Ball _ ) ->
            True

        ( Border, Border ) ->
            True

        ( Player, Player ) ->
            True

        ( Space, Space ) ->
            True

        ( Trail, Trail ) ->
            True

        _ ->
            False


isSpace : Maybe Cell -> Bool
isSpace mc =
    case mc of
        Nothing ->
            False

        Just c ->
            c.shape == Space


player : Color -> Int -> Int -> Cell
player c x y =
    Cell x y c Player


space : Color -> Int -> Int -> Cell
space c x y =
    Cell x y c Space


trail : Color -> Int -> Int -> Cell
trail c x y =
    Cell x y c Trail


size : Int
size =
    10
