module Grid
    exposing
        ( Colors
        , Grid
        , Size
        , Vertex
        , VertexLine
        , animate
        , clearTrail
        , conquer
        , inAnimation
        , init
        , initAnimation
        , movePlayer
        , nextBallPositions
        , update
        )

import Animation exposing (Animation)
import Cell exposing (Cell, Direction(..), Shape(..))
import Color exposing (Color)
import Dict exposing (Dict)
import Keys exposing (KeyName(..))
import Maybe.Extra as EMaybe
import Messages exposing (Msg(..))
import Random exposing (Generator, Seed)
import Set exposing (Set)
import Task exposing (Task)
import Time exposing (Time)


{-| `Grid` is about capturing all things required for the renderer to visualize
the next game state.

The game grid coordinate (0,0) is top/left corner, both axis contain only
positive values.

The `priorPlayer` keeps the shape/color before active player position was updated
to that position, but the grid coordinates match also the current player position.

-}
type alias Grid =
    { animation : Maybe Animation
    , animationDelta : Float
    , balls : List (Maybe Cell)
    , cells : Dict ( Int, Int ) (Maybe Cell)
    , colors : Colors
    , priorPlayer : ( ( Int, Int ), Maybe Cell )
    , size : Size
    , trail : List ( Int, Int, KeyName )
    }


{-| Main game object colors.
-}
type alias Colors =
    { ball : Color
    , border : Color
    , conquest : Color
    , player : Color
    , space : Color
    , trail : Color
    }


{-| Size of the game board (not the rendered canvas size!)
-}
type alias Size =
    { height : Int
    , width : Int
    }


type alias Vertex =
    { x : Int
    , y : Int
    , lines : List VertexLine
    }


type VertexLine
    = North
    | South
    | East
    | West


animate : Time -> Grid -> Grid
animate systemTick grid =
    { grid
        | animationDelta =
            grid.animation
                |> Maybe.map (\a -> Animation.animate systemTick a)
                |> Maybe.withDefault 0
    }


ballCollision : Maybe Cell -> ( ( Int, Int ), Maybe Cell ) -> Bool
ballCollision nextCell priorCell =
    Cell.isTrail nextCell
        || (Cell.isPlayer nextCell && not (playerInsideBorder nextCell priorCell))


conquer : Grid -> Grid
conquer grid =
    conquerTrail grid
        |> conquerSpace


conquerSpace : Grid -> Grid
conquerSpace grid =
    let
        outline =
            Debug.log "outline cells" (findOutlineCells grid)

        vertexes =
            Debug.log "vertex cells" (findVertexes grid outline)

        polygons =
            findPolygons vertexes

        --        lines =
        --            Debug.log "lines" (findLines vertexes)
        --        lineCount =
        --            Debug.log "line count" (List.length lines)
        debugOutline =
            Set.foldl
                (\key accum ->
                    case Dict.get key grid.cells |> EMaybe.join of
                        Just c ->
                            Dict.update key (\_ -> Just <| Just { c | color = Color.green }) accum

                        Nothing ->
                            accum
                )
                grid.cells
                outline

        debugVertexes vgrid =
            List.foldl
                (\{ x, y } accum ->
                    case Dict.get ( x, y ) vgrid |> EMaybe.join of
                        Just c ->
                            Dict.update ( x, y ) (\_ -> Just <| Just { c | color = Color.blue }) accum

                        Nothing ->
                            accum
                )
                vgrid
                vertexes
    in
    { grid
        | cells = debugOutline |> debugVertexes
    }


conquerTrail : Grid -> Grid
conquerTrail grid =
    { grid
        | cells =
            grid.trail
                |> List.foldl
                    (\( x, y, _ ) accum ->
                        Dict.insert
                            ( x, y )
                            (Cell.conquest grid.colors.conquest x y |> Just)
                            accum
                    )
                    grid.cells
        , trail = []
    }


clearTrail : Grid -> Grid
clearTrail grid =
    let
        ( ( px, py ), _ ) =
            grid.priorPlayer
    in
    { grid
        | cells =
            grid.trail
                |> List.foldl
                    (\( x, y, _ ) accum -> Dict.insert ( x, y ) Nothing accum)
                    grid.cells
                |> (\cells -> Dict.insert ( px, py ) Nothing cells)
        , trail = []
    }
        |> initPlayer


{-| @private
Create an empty grid with specified size: height/width
-}
empty : Grid
empty =
    { animation = Nothing
    , animationDelta = toFloat Cell.size
    , balls = []
    , cells = Dict.empty
    , colors =
        { ball = Color.orange
        , border = Color.lightBlue
        , conquest = Color.blue
        , player = Color.darkRed
        , space = Color.white
        , trail = Color.darkYellow
        }
    , priorPlayer = ( ( -1, -1 ), Nothing )
    , size = { height = 46, width = 64 }
    , trail = []
    }


{-| @private
Find all cells (points) that make up the 2 polygons with a common trail.
-}
findOutlineCells : Grid -> Set ( Int, Int )
findOutlineCells grid =
    let
        sidePoints ( x, y ) =
            [ ( x + 1, y )
            , ( x - 1, y )
            , ( x, y - 1 )
            , ( x, y + 1 )
            , ( x + 1, y - 1 )
            , ( x + 1, y + 1 )
            , ( x - 1, y + 1 )
            , ( x - 1, y - 1 )
            ]

        freeSides ( x, y ) =
            sidePoints ( x, y )
                |> List.foldl
                    (\pnt accum ->
                        case Dict.get pnt grid.cells of
                            Just mc ->
                                if Cell.isSpace mc then
                                    pnt :: accum
                                else
                                    accum

                            Nothing ->
                                accum
                    )
                    []

        isOutlineCell sides =
            List.length sides >= 1
    in
    grid.cells
        |> Dict.filter (\_ mc -> Cell.isPlayer mc || Cell.isBorder mc || Cell.isConquest mc)
        |> Dict.foldl
            (\key _ accum ->
                if freeSides key |> isOutlineCell then
                    Set.insert key accum
                else
                    accum
            )
            Set.empty


{-| @private

A trail left by the player, dividing the empty area will always create 2 polygons.
There are 2 vertexes, at each end of the trail, that have 3 line connections,
while the rest have two connections (`VertexLine`).

        1) find trail vertexes
        2) for each trail vertex
            2.1) trace next vertex in clockwise direction not on the trail
            2.2) is (the new) vertex equal to the other trail vertex (crossing) ?
                2.2.1) no -> goto 2.1
                2.2.2) yes, continue
            2.3) trace the next vertex on the trail
            2.4) is (the new) vertex equal to the other (starting) trail vertex ?
                2.4.1) no -> goto 2.3
                2.2.2) yes -> polygon complete

-}
findPolygons : List Vertex -> ( List Vertex, List Vertex )
findPolygons vertexes =
    let
        ( trailv1, trailv2 ) =
            Debug.log "Trail vertexes"
                (vertexes
                    |> List.filter (\r -> List.length r.lines == 3)
                    |> (\tvl ->
                            case tvl of
                                t1 :: t2 :: [] ->
                                    ( t1, t2 )

                                _ ->
                                    Debug.crash "Something is wrong in vertex logic, expecting 2 trail vertexes"
                       )
                )

        poly1 =
            Debug.log "poly1"
                (directionClockwise trailv1
                    |> nextVertex vertexes trailv1
                    |> tracePath vertexes [] trailv2
                    |> (\accum ->
                            directionTrail trailv2
                                |> nextVertex vertexes trailv2
                                |> tracePath vertexes accum trailv1
                       )
                )

        poly2 =
            Debug.log "poly2"
                (directionClockwise trailv2
                    |> nextVertex vertexes trailv2
                    |> tracePath vertexes [] trailv1
                    |> (\accum ->
                            directionTrail trailv1
                                |> nextVertex vertexes trailv1
                                |> tracePath vertexes accum trailv2
                       )
                )
    in
    ( poly1, poly2 )


{-| @private
Go from a `List VertexLine` e.g. [North-South-East] to sorted "-East-North-South"
-}
directionString : List VertexLine -> String
directionString directions =
    directions
        |> List.map Basics.toString
        |> List.sort
        |> List.foldl (\ds a -> String.append a ("-" ++ ds)) ""


{-| @private
-}
directionPredicate : VertexLine -> Vertex -> Vertex -> Bool
directionPredicate direction start current =
    case direction of
        East ->
            current.x > start.x && current.y == start.y

        North ->
            current.x == start.x && current.y < start.y

        South ->
            current.x == start.x && current.y > start.y

        West ->
            current.x < start.x && current.y == start.y


{-| @private
-}
directionSorter : VertexLine -> Vertex -> Vertex -> Order
directionSorter direction va vb =
    case direction of
        East ->
            Basics.compare va.x vb.x

        North ->
            case Basics.compare va.y vb.y of
                EQ ->
                    EQ

                GT ->
                    LT

                LT ->
                    GT

        South ->
            Basics.compare va.y vb.y

        West ->
            case Basics.compare va.x vb.x of
                EQ ->
                    EQ

                GT ->
                    LT

                LT ->
                    GT


{-| @private
-}
directionClockwise : Vertex -> VertexLine
directionClockwise v =
    case directionString v.lines of
        "-East-North-South" ->
            North

        "-East-North-West" ->
            West

        "-East-South-West" ->
            East

        "-North-South-West" ->
            South

        _ ->
            Debug.crash "Could not find clockwise trace direction from trail's vertex"


{-| @private
-}
directionTrail : Vertex -> VertexLine
directionTrail v =
    case directionString v.lines of
        "-East-North-South" ->
            East

        "-East-North-West" ->
            North

        "-East-South-West" ->
            South

        "-North-South-West" ->
            West

        _ ->
            Debug.crash "Could not find clockwise trace direction from trail's vertex"


{-| @private
Except for 2 trail vertexes, all other vertexes have 2 line connections.
Eliminate the (line) direction from which "a tracer" arrived and returned the
next (remaining) direction.
-}
nextDirection : ( Vertex, VertexLine ) -> Maybe VertexLine
nextDirection ( vertex, prevdir ) =
    let
        arraival =
            case prevdir of
                East ->
                    West

                North ->
                    South

                South ->
                    North

                West ->
                    East
    in
    vertex.lines
        |> List.filter (\d -> d /= arraival)
        |> List.head


{-| @private
Given a vertex point and direction, trace to the next vertex point and arraival direction.
-}
nextVertex : List Vertex -> Vertex -> VertexLine -> Maybe ( Vertex, VertexLine )
nextVertex vertexes start direction =
    vertexes
        |> List.filter (directionPredicate direction start)
        |> List.sortWith (directionSorter direction)
        |> List.head
        |> Maybe.map (\v -> ( v, direction ))


{-| @private
-}
tracePath : List Vertex -> List Vertex -> Vertex -> Maybe ( Vertex, VertexLine ) -> List Vertex
tracePath vertexes accum crossing mvd =
    case mvd of
        Nothing ->
            let
                _ =
                    Debug.log "Missing next vertex and previous direction."
            in
            []

        Just ( current, previousDirection ) ->
            case current == crossing of
                True ->
                    current :: accum

                False ->
                    let
                        next =
                            nextDirection ( current, previousDirection )
                                |> Maybe.map (nextVertex vertexes current)
                                |> EMaybe.join
                    in
                    tracePath vertexes (current :: accum) crossing next


{-| @private

Base vertex combinations to be searched for every given point: (x, y).

        S,E           S,W
         _ _         _ _
        |_|_|-------|_|_|
        |_|           |_|
        .             .
        .             .
         _             _
        |_|_         _|_|
        |_|_|-------|_|_|
        N,E           N,W

-}
findVertexes : Grid -> Set ( Int, Int ) -> List Vertex
findVertexes grid outlineCells =
    let
        lineMap idx =
            case idx of
                1 ->
                    North

                2 ->
                    South

                3 ->
                    East

                4 ->
                    West

                _ ->
                    Debug.crash "Invalid vertex line index."

        vertexChecks ( x, y ) =
            [ { outline = ( ( x, y - 1 ), ( x + 1, y ) )
              , space = [ ( x + 1, y - 1 ) ]
              , lines = [ 1, 3 ]
              }
            , { outline = ( ( x, y - 1 ), ( x + 1, y ) )
              , space = [ ( x - 1, y ), ( x - 1, y + 1 ), ( x, y + 1 ) ]
              , lines = [ 1, 3 ]
              }
            , { outline = ( ( x + 1, y ), ( x, y + 1 ) )
              , space = [ ( x + 1, y + 1 ) ]
              , lines = [ 2, 3 ]
              }
            , { outline = ( ( x + 1, y ), ( x, y + 1 ) )
              , space = [ ( x - 1, y ), ( x - 1, y - 1 ), ( x, y - 1 ) ]
              , lines = [ 2, 3 ]
              }
            , { outline = ( ( x, y + 1 ), ( x - 1, y ) )
              , space = [ ( x - 1, y + 1 ) ]
              , lines = [ 2, 4 ]
              }
            , { outline = ( ( x, y + 1 ), ( x - 1, y ) )
              , space = [ ( x, y - 1 ), ( x + 1, y - 1 ), ( x + 1, y ) ]
              , lines = [ 2, 4 ]
              }
            , { outline = ( ( x - 1, y ), ( x, y - 1 ) )
              , space = [ ( x - 1, y - 1 ) ]
              , lines = [ 1, 4 ]
              }
            , { outline = ( ( x - 1, y ), ( x, y - 1 ) )
              , space = [ ( x + 1, y ), ( x + 1, y + 1 ), ( x, y + 1 ) ]
              , lines = [ 1, 4 ]
              }
            ]

        checkOutline ( xy1, xy2 ) =
            Set.member xy1 outlineCells && Set.member xy2 outlineCells

        checkSpace coordPairs =
            List.length coordPairs
                == (coordPairs
                        |> List.filter (\xy -> Dict.get xy grid.cells |> EMaybe.join |> Cell.isSpace)
                        |> List.length
                   )

        pointChecks ( x, y ) =
            vertexChecks ( x, y )
                |> List.filter (\vrec -> checkOutline vrec.outline && checkSpace vrec.space)
                |> List.map (\vrec -> Set.fromList vrec.lines)
                |> List.foldl (\s accum -> Set.union accum s) Set.empty
                |> (\s -> Set.toList s |> List.map lineMap)

        isVertex checks =
            List.length checks >= 1
    in
    outlineCells
        |> Set.foldl
            (\xy accum ->
                let
                    pc =
                        pointChecks xy

                    ( x, y ) =
                        xy
                in
                if isVertex pc then
                    Vertex x y pc :: accum
                else
                    accum
            )
            []


{-| @private
Construct lines from vertexes, horizontal an vertical.
-}
findLines : List Vertex -> List (List Vertex)
findLines vertexes =
    let
        vertexSort primaryFn secondaryFn v1 v2 =
            let
                p1 =
                    primaryFn v1

                s1 =
                    secondaryFn v1

                p2 =
                    primaryFn v2

                s2 =
                    secondaryFn v2
            in
            if p1 < p2 then
                LT
            else if p1 == p2 && s1 < s2 then
                LT
            else if p1 > p2 then
                GT
            else if p1 == p2 && s1 > s2 then
                GT
            else
                EQ

        groupPrimaries primaryFn vlist =
            List.foldl
                (\v accum ->
                    case accum of
                        [] ->
                            [ v ] :: accum

                        sublist :: rest ->
                            List.head sublist
                                |> Maybe.map
                                    (\p ->
                                        if primaryFn v == primaryFn p then
                                            (v :: sublist) :: rest
                                        else
                                            [ v ] :: accum
                                    )
                                |> Maybe.withDefault accum
                )
                []
                vlist

        groupLines primaries =
            List.foldl
                (\pgroup accum ->
                    if List.length pgroup > 1 then
                        accum
                            ++ List.foldl
                                (\v a ->
                                    case a of
                                        [] ->
                                            [ v ] :: a

                                        lgrp :: rest ->
                                            if List.length lgrp == 2 then
                                                List.head lgrp
                                                    |> Maybe.map (\p -> [ v, p ] :: a)
                                                    |> Maybe.withDefault a
                                            else
                                                (v :: lgrp) :: rest
                                )
                                []
                                pgroup
                    else
                        accum
                )
                []
                primaries
    in
    (List.sortWith (vertexSort .y .x) vertexes
        |> groupPrimaries .y
        |> groupLines
    )
        ++ (List.sortWith (vertexSort .x .y) vertexes
                |> groupPrimaries .x
                |> groupLines
           )


inAnimation : Time -> Grid -> Bool
inAnimation systemTick grid =
    grid.animation
        |> Maybe.map (\a -> Animation.isDone systemTick a |> not)
        |> Maybe.withDefault False


{-| Initialize game grid for a given level.
-}
init : Int -> (List (Maybe Cell) -> msg) -> ( Grid, Cmd msg )
init level posMsg =
    let
        grid =
            empty
                |> space
                |> xBorder 0
                |> (\g -> xBorder (g.size.height - 1) g)
                |> yBorder 0
                |> (\g -> yBorder (g.size.width - 1) g)
                |> initPlayer
    in
    ( grid
    , initBalls level grid
        |> Task.map
            (\( ballPositions, _ ) ->
                ballPositions
                    |> List.indexedMap
                        (\i ( x, y ) ->
                            Cell.ball
                                grid.colors.ball
                                x
                                y
                                (Cell.direction <| (i + 1) % 4)
                                |> Just
                        )
            )
        |> Task.perform posMsg
    )


initAnimation : Time -> Grid -> Grid
initAnimation systemTick grid =
    let
        a =
            Animation.animation systemTick
                |> Animation.duration 32
                |> Animation.from 0
                |> Animation.to (toFloat Cell.size)
    in
    { grid
        | animation = Just a
        , animationDelta = Animation.animate systemTick a
    }


initPlayer : Grid -> Grid
initPlayer grid =
    let
        px =
            grid.size.width // 2

        py =
            grid.size.height - 1

        priorCell =
            Dict.get ( px, py ) grid.cells
                |> EMaybe.join

        player =
            Cell.player grid.colors.player px py
    in
    { grid
        | cells =
            Dict.update
                ( px, py )
                (\_ -> Just <| Just player)
                grid.cells
        , priorPlayer = ( ( px, py ), priorCell )
    }


{-| @private
Initialize a given number of ball positions.
-}
initBalls : Int -> Grid -> Task x ( List ( Int, Int ), Seed )
initBalls count g =
    Time.now
        |> Task.map
            (\t ->
                Basics.round t
                    |> Random.initialSeed
                    |> Random.step (positionGenerator count g)
            )


movePlayer : KeyName -> Grid -> ( Grid, Cmd Msg )
movePlayer keyName grid =
    let
        ( ( cx, cy ), pc ) =
            grid.priorPlayer

        ( px, py ) =
            case keyName of
                KeyArrowDown ->
                    ( cx, cy + 1 )

                KeyArrowLeft ->
                    ( cx - 1, cy )

                KeyArrowRight ->
                    ( cx + 1, cy )

                KeyArrowUp ->
                    ( cx, cy - 1 )
    in
    if Dict.member ( px, py ) grid.cells then
        let
            priorCell =
                Dict.get ( px, py ) grid.cells
                    |> EMaybe.join

            ( restoreCell, updatedTrail ) =
                if Cell.isSpace pc then
                    ( Cell.trail grid.colors.trail cx cy |> Just
                    , ( cx, cy, keyName ) :: grid.trail
                    )
                else
                    ( pc
                    , grid.trail
                    )
        in
        if Cell.isTrail priorCell || (Cell.isSpace pc && Cell.isBorder priorCell && List.isEmpty grid.trail) then
            ( grid
            , Task.succeed TakeLife |> Task.perform identity
            )
        else
            ( { grid
                | cells =
                    grid.cells
                        |> Dict.update ( cx, cy ) (\_ -> Just <| restoreCell)
                        |> Dict.update
                            ( px, py )
                            (\_ -> Just <| Just <| Cell.player grid.colors.player px py)
                , priorPlayer = ( ( px, py ), priorCell )
                , trail = updatedTrail
              }
            , if (Cell.isBorder priorCell || Cell.isConquest priorCell) && not (List.isEmpty grid.trail) then
                Task.succeed Conquer |> Task.perform identity
              else
                Cmd.none
            )
    else
        ( grid
        , Cmd.none
        )


nextBallPositions : Grid -> ( List (Maybe Cell), Cmd Msg )
nextBallPositions grid =
    grid.balls
        |> List.map
            (Maybe.map
                (\c ->
                    case c.shape of
                        Ball direction ->
                            case direction of
                                NE ->
                                    nextBallNE grid c

                                NW ->
                                    nextBallNW grid c

                                SE ->
                                    nextBallSE grid c

                                SW ->
                                    nextBallSW grid c

                        _ ->
                            ( c, Cmd.none )
                )
            )
        |> List.foldl
            (\mc ( balls, finalCmd ) ->
                ( (mc |> Maybe.map (\( c, _ ) -> c)) :: balls
                , mc
                    |> Maybe.map
                        (\( _, cmd ) ->
                            if finalCmd == Cmd.none && cmd /= Cmd.none then
                                cmd
                            else
                                finalCmd
                        )
                    |> Maybe.withDefault Cmd.none
                )
            )
            ( [], Cmd.none )


{-| @private
-}
nextBallNE : Grid -> Cell -> ( Cell, Cmd Msg )
nextBallNE grid c =
    let
        nextCell =
            Dict.get ( c.cellX + 1, c.cellY - 1 ) grid.cells
                |> EMaybe.join

        leftCell =
            Dict.get ( c.cellX - 1, c.cellY - 1 ) grid.cells
                |> EMaybe.join

        rightCell =
            Dict.get ( c.cellX + 1, c.cellY + 1 ) grid.cells
                |> EMaybe.join
    in
    case ballCollision nextCell grid.priorPlayer of
        False ->
            if Cell.isSpace nextCell then
                ( { c | cellX = c.cellX + 1, cellY = c.cellY - 1 }
                , Cmd.none
                )
            else if Cell.isSpace leftCell then
                ( { c | cellX = c.cellX - 1, cellY = c.cellY - 1, shape = Ball NW }
                , Cmd.none
                )
            else if Cell.isSpace rightCell then
                ( { c | cellX = c.cellX + 1, cellY = c.cellY + 1, shape = Ball SE }
                , Cmd.none
                )
            else
                ( { c | cellX = c.cellX - 1, cellY = c.cellY + 1, shape = Ball SW }
                , Cmd.none
                )

        True ->
            ( c
            , Task.succeed TakeLife
                |> Task.perform identity
            )


{-| @private
-}
nextBallNW : Grid -> Cell -> ( Cell, Cmd Msg )
nextBallNW grid c =
    let
        nextCell =
            Dict.get ( c.cellX - 1, c.cellY - 1 ) grid.cells
                |> EMaybe.join

        leftCell =
            Dict.get ( c.cellX - 1, c.cellY + 1 ) grid.cells
                |> EMaybe.join

        rightCell =
            Dict.get ( c.cellX + 1, c.cellY - 1 ) grid.cells
                |> EMaybe.join
    in
    case ballCollision nextCell grid.priorPlayer of
        False ->
            if Cell.isSpace nextCell then
                ( { c | cellX = c.cellX - 1, cellY = c.cellY - 1 }
                , Cmd.none
                )
            else if Cell.isSpace leftCell then
                ( { c | cellX = c.cellX - 1, cellY = c.cellY + 1, shape = Ball SW }
                , Cmd.none
                )
            else if Cell.isSpace rightCell then
                ( { c | cellX = c.cellX + 1, cellY = c.cellY - 1, shape = Ball NE }
                , Cmd.none
                )
            else
                ( { c | cellX = c.cellX + 1, cellY = c.cellY + 1, shape = Ball SE }
                , Cmd.none
                )

        True ->
            ( c
            , Task.succeed TakeLife
                |> Task.perform identity
            )


{-| @private
-}
nextBallSE : Grid -> Cell -> ( Cell, Cmd Msg )
nextBallSE grid c =
    let
        nextCell =
            Dict.get ( c.cellX + 1, c.cellY + 1 ) grid.cells
                |> EMaybe.join

        leftCell =
            Dict.get ( c.cellX + 1, c.cellY - 1 ) grid.cells
                |> EMaybe.join

        rightCell =
            Dict.get ( c.cellX - 1, c.cellY + 1 ) grid.cells
                |> EMaybe.join
    in
    case ballCollision nextCell grid.priorPlayer of
        False ->
            if Cell.isSpace nextCell then
                ( { c | cellX = c.cellX + 1, cellY = c.cellY + 1 }
                , Cmd.none
                )
            else if Cell.isSpace leftCell then
                ( { c | cellX = c.cellX + 1, cellY = c.cellY - 1, shape = Ball NE }
                , Cmd.none
                )
            else if Cell.isSpace rightCell then
                ( { c | cellX = c.cellX - 1, cellY = c.cellY + 1, shape = Ball SW }
                , Cmd.none
                )
            else
                ( { c | cellX = c.cellX - 1, cellY = c.cellY - 1, shape = Ball NW }
                , Cmd.none
                )

        True ->
            ( c
            , Task.succeed TakeLife
                |> Task.perform identity
            )


{-| @private
-}
nextBallSW : Grid -> Cell -> ( Cell, Cmd Msg )
nextBallSW grid c =
    let
        nextCell =
            Dict.get ( c.cellX - 1, c.cellY + 1 ) grid.cells
                |> EMaybe.join

        leftCell =
            Dict.get ( c.cellX + 1, c.cellY + 1 ) grid.cells
                |> EMaybe.join

        rightCell =
            Dict.get ( c.cellX - 1, c.cellY - 1 ) grid.cells
                |> EMaybe.join
    in
    case ballCollision nextCell grid.priorPlayer of
        False ->
            if Cell.isSpace nextCell then
                ( { c | cellX = c.cellX - 1, cellY = c.cellY + 1 }
                , Cmd.none
                )
            else if Cell.isSpace leftCell then
                ( { c | cellX = c.cellX + 1, cellY = c.cellY + 1, shape = Ball SE }
                , Cmd.none
                )
            else if Cell.isSpace rightCell then
                ( { c | cellX = c.cellX - 1, cellY = c.cellY - 1, shape = Ball NW }
                , Cmd.none
                )
            else
                ( { c | cellX = c.cellX + 1, cellY = c.cellY - 1, shape = Ball NE }
                , Cmd.none
                )

        True ->
            ( c
            , Task.succeed TakeLife
                |> Task.perform identity
            )


{-| @private
-}
playerInsideBorder : Maybe Cell -> ( ( Int, Int ), Maybe Cell ) -> Bool
playerInsideBorder nextCell ( _, priorCell ) =
    case ( nextCell, priorCell ) of
        ( Just c, Just pc ) ->
            (c.shape == Player)
                && (pc.shape == Border || pc.shape == Conquest)
                && (c.cellX == pc.cellX)
                && (c.cellY == pc.cellY)

        _ ->
            False


{-| @private
Given a count generate number of random `Grid` coordinates for placeing game objects.
-}
positionGenerator : Int -> Grid -> Generator (List ( Int, Int ))
positionGenerator count g =
    Random.list count <|
        Random.pair
            (Random.int 2 (g.size.width - 4))
            (Random.int 2 (g.size.height - 4))


{-| @private
-}
setToCell : List (Maybe Cell) -> Grid -> Grid
setToCell cells grid =
    cells
        |> List.foldl
            (\mc cells ->
                mc
                    |> Maybe.map
                        (\c ->
                            Dict.update ( c.cellX, c.cellY ) (\_ -> Just (Just c)) cells
                        )
                    |> Maybe.withDefault cells
            )
            grid.cells
        |> (\newCells -> { grid | cells = newCells })


{-| @private
-}
setToShape : Shape -> Color -> Grid -> List ( Int, Int ) -> Grid
setToShape s c grid coords =
    coords
        |> List.foldl
            (\( x, y ) cells ->
                Dict.update ( x, y ) (\_ -> Just <| (Cell x y c s |> Just)) cells
            )
            grid.cells
        |> (\newCells -> { grid | cells = newCells })


setToSpace : Grid -> List ( Int, Int ) -> Grid
setToSpace grid coords =
    coords
        |> List.foldl
            (\( x, y ) cells ->
                Dict.update ( x, y ) (\_ -> Just Nothing) cells
            )
            grid.cells
        |> (\newCells -> { grid | cells = newCells })


{-| @private
-}
shapePositions : Shape -> Grid -> List ( Int, Int )
shapePositions s g =
    g.cells
        |> Dict.filter
            (\_ c ->
                Maybe.map (\cell -> Cell.equal s cell.shape) c
                    |> Maybe.withDefault False
            )
        |> Dict.keys


{-| @private
Initialize the empty space i.e. the board area to be captured by the player while
evading collisions with the balls.
-}
space : Grid -> Grid
space g =
    List.repeat (g.size.height - 2) Nothing
        |> List.repeat (g.size.width - 2)
        |> List.indexedMap (\x cols -> List.indexedMap (\y _ -> ( x + 1, y + 1 )) cols)
        |> List.concat
        |> List.foldl (\( x, y ) -> Dict.insert ( x, y ) Nothing) g.cells
        |> (\newCells -> { g | cells = newCells })


{-| Partition and update `Cell`s in `Shape` clusters.
-}
update : Grid -> List (Maybe Cell) -> Grid
update grid cells =
    [ Ball NE
    , Border
    , Conquest
    , Player
    , Trail
    ]
        |> List.map
            (\shape ->
                cells
                    |> List.filter
                        (\c ->
                            Maybe.map (\cell -> Cell.equal shape cell.shape) c
                                |> Maybe.withDefault False
                        )
            )
        |> List.foldl
            (\sameCells g ->
                case
                    List.head sameCells
                        |> EMaybe.join
                        |> Maybe.map .shape
                of
                    Nothing ->
                        g

                    Just (Ball direction) ->
                        shapePositions (Ball direction) g
                            |> setToSpace g
                            |> setToCell sameCells
                            |> (\ng -> { ng | balls = sameCells })

                    Just Border ->
                        g

                    Just Conquest ->
                        g

                    Just Player ->
                        g

                    Just Trail ->
                        g
            )
            grid


{-| @private
Given an X coordinate initialize a horizontal game board's border.
-}
xBorder : Int -> Grid -> Grid
xBorder y g =
    List.repeat g.size.width (Cell.border g.colors.border)
        |> List.indexedMap (\i blockFn -> blockFn i y)
        |> List.foldl (\c -> Dict.insert ( c.cellX, c.cellY ) (Just c)) g.cells
        |> (\newCells -> { g | cells = newCells })


{-| @private
Given an Y coordinate initialize a vertical game board's border.
-}
yBorder : Int -> Grid -> Grid
yBorder x g =
    List.repeat g.size.height (Cell.border g.colors.border)
        |> List.indexedMap (\i blockFn -> blockFn x i)
        |> List.foldl (\c -> Dict.insert ( c.cellX, c.cellY ) (Just c)) g.cells
        |> (\newCells -> { g | cells = newCells })
