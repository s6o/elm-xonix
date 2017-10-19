module Cell
    exposing
        ( Cell
        , Direction(..)
        , Shape(..)
        , ball
        , border
        , direction
        , dxdy
        , equal
        , isSpace
        , player
        , size
        , trail
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


{-| Calculate a new Elm canvas point given a direction, delta and a destination point.

    NW      ^ y+    NE
            |
            |
    x-      |
    --------|-------->
            |       x+
            |
            |
    SW   y- |       SE

-}
dxdy : Direction -> Float -> ( Float, Float ) -> ( Float, Float )
dxdy d delta ( destX, destY ) =
    let
        cellDelta =
            toFloat size - delta
    in
    case d of
        NE ->
            ( destX - cellDelta
            , destY - cellDelta
            )

        NW ->
            ( destX + cellDelta
            , destY - cellDelta
            )

        SE ->
            ( destX - cellDelta
            , destY + cellDelta
            )

        SW ->
            ( destX + cellDelta
            , destY + cellDelta
            )


equal : Shape -> Shape -> Bool
equal s1 s2 =
    case ( s1, s2 ) of
        ( Ball _, Ball _ ) ->
            True

        ( Border, Border ) ->
            True

        ( Player, Player ) ->
            True

        ( Trail, Trail ) ->
            True

        _ ->
            False


isSpace : Maybe Cell -> Bool
isSpace mc =
    case mc of
        Nothing ->
            True

        Just _ ->
            False


player : Color -> Int -> Int -> Cell
player c x y =
    Cell x y c Player


trail : Color -> Int -> Int -> Cell
trail c x y =
    Cell x y c Trail


size : Int
size =
    10
