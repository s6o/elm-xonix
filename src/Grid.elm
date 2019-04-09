module Grid
    exposing
        ( Colors
        , Grid
        , Size
        , Vertex
        , VertexConnection
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
import Array
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
    , c : List VertexConnection
    }


type VertexConnection
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



{- Realized that, the border needs to shift to were the (new) trail was cut.
   The old (outer most) border will become part of conquered space.

   Thus, first conquerSpace:
       * find outline points
       * find lines from outline points
       * find vertexes from lines
       * find the 2 polygons via vertexes (and lines)
       * check which of the 2 polygons can be filled (contain no balls)
       * fill (conquer) polygons without balls - in fill, remember to exclude trail
       * fill trail to be the new border
-}


conquer : Grid -> Grid
conquer grid =
    --conquerTrail grid
    conquerSpace grid


conquerSpace : Grid -> Grid
conquerSpace grid =
    let
        outline =
            Debug.log "outline cells" (findOutlineCells grid)

        lines =
            Debug.log "lines" (findLines outline)

        vertexes =
            Debug.log "vertex cells" (findVertexes2 grid lines)

        polygons =
            findPolygons grid vertexes

        polyLines =
            let
                ( poly1, poly2 ) =
                    polygons
            in
            ( Debug.log "poly 1 lines" (findVerticalLines poly1)
            , Debug.log "poly 2 lines" (findVerticalLines poly2)
            )

        polysToFill =
            findPolygonsToFill polyLines grid.balls

        debugOutline ol ogrid =
            Set.foldl
                (\key accum ->
                    case Dict.get key grid.cells |> EMaybe.join of
                        Just c ->
                            Dict.update key (\_ -> Just <| Just { c | color = Color.green }) accum

                        Nothing ->
                            accum
                )
                ogrid.cells
                ol

        debugVertexes vtxs vgrid =
            List.foldl
                (\{ x, y } accum ->
                    case Dict.get ( x, y ) vgrid |> EMaybe.join of
                        Just c ->
                            Dict.update ( x, y ) (\_ -> Just <| Just { c | color = Color.blue }) accum

                        Nothing ->
                            accum
                )
                vgrid
                vtxs
    in
    grid
        {-
           |> (\g -> { g | cells = debugOutline outline g |> debugVertexes vertexes })
           |> (\g -> { g | cells = g.cells |> Dict.map (\( x, y ) mc -> mc |> Maybe.map (\c -> { c | color = Color.darkGrey })) })
        -}
        |> (\g -> { g | cells = fillPolygons polysToFill g, trail = [] })


conquerTrail : Grid -> Grid
conquerTrail grid =
    { grid
        | cells =
            grid.trail
                |> List.foldl
                    (\( x, y, _ ) accum ->
                        Dict.insert
                            ( x, y )
                            (Cell.border grid.colors.border x y |> Just)
                            accum
                    )
                    grid.cells

        --        , trail = []
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



--- IMPL. Details


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
        |> Dict.filter (\_ mc -> Cell.isPlayer mc || Cell.isBorder mc || Cell.isTrail mc)
        |> Dict.foldl
            (\key _ accum ->
                if freeSides key |> isOutlineCell then
                    Set.insert key accum
                else
                    accum
            )
            Set.empty


{-| @private
Find (unique) vertexes with `VertexConnection`s from lines.
-}
findVertexes2 : Grid -> List ( Vertex, Vertex ) -> List Vertex
findVertexes2 grid lines =
    let
        directions x y =
            [ ( x + 1, y, East )
            , ( x - 1, y, West )
            , ( x, y + 1, South )
            , ( x, y - 1, North )
            ]

        trailCells =
            grid.cells
                |> Dict.filter (\_ mc -> Cell.isPlayer mc || Cell.isBorder mc || Cell.isConquest mc || Cell.isTrail mc)
    in
    lines
        |> List.map (\( v1, v2 ) -> [ ( v1.x, v1.y ), ( v2.x, v2.y ) ])
        |> List.concat
        |> Set.fromList
        |> Set.toList
        |> List.map
            (\( x, y ) ->
                directions x y
                    |> List.foldl
                        (\( dx, dy, c ) acc ->
                            trailCells
                                |> Dict.get ( dx, dy )
                                |> Maybe.map (\_ -> c :: acc)
                                |> Maybe.withDefault acc
                        )
                        []
                    |> (\connections -> Vertex x y connections)
            )
        |> (\result ->
                let
                    _ =
                        Debug.log "Vertex count" (List.length result)
                in
                result
           )


{-| @private
Find vertex point candidates for polygons starts.
-}
vertexPointsFromTrail : ( Maybe ( Int, Int, KeyName ), Maybe ( Int, Int, KeyName ) ) -> ( Maybe ( Int, Int ), Maybe ( Int, Int ) )
vertexPointsFromTrail ( tailEnd, tailStart ) =
    ( case tailEnd of
        Nothing ->
            Nothing

        Just ( tx, ty, _ ) ->
            Just ( tx, ty )
      {-
         case key of
             KeyArrowDown ->
                 Just ( tx, ty + 1 )

             KeyArrowLeft ->
                 Just ( tx - 1, ty )

             KeyArrowRight ->
                 Just ( tx + 1, ty )

             KeyArrowUp ->
                 Just ( tx, ty - 1 )
      -}
    , case tailStart of
        Nothing ->
            Nothing

        Just ( tx, ty, _ ) ->
            Just ( tx, ty )
      {-
         case key of
             KeyArrowDown ->
                 Just ( tx, ty - 1 )

             KeyArrowLeft ->
                 Just ( tx + 1, ty )

             KeyArrowRight ->
                 Just ( tx - 1, ty )

             KeyArrowUp ->
                 Just ( tx, ty + 1 )
      -}
    )


{-| @private

A trail left by the player, dividing the empty area will always create 2 polygons.
There are 2 vertexes, at each end of the trail, that have 3 line connections,
while the rest have two connections (`VertexLine`).

      * 1) find trail vertexes
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
findPolygons : Grid -> List Vertex -> ( List Vertex, List Vertex )
findPolygons grid vertexes =
    let
        vertexCandidates =
            ( List.head grid.trail
            , grid.trail |> List.reverse |> List.head
            )
                |> Debug.log "Vertex Points Trail"
                >> vertexPointsFromTrail
                |> (\( tvEnd, tvStart ) ->
                        Maybe.map2 (\tv1 tv2 -> [ tv1, tv2 ])
                            tvEnd
                            tvStart
                            |> Maybe.withDefault []
                   )
                |> List.foldl
                    (\( tx, ty ) acc ->
                        vertexes
                            |> List.filter (\v -> tx == v.x && ty == v.y)
                            |> List.head
                            |> Maybe.map (\u -> u :: acc)
                            |> Maybe.withDefault acc
                    )
                    []

        ( trailv1, trailv2 ) =
            Debug.log "Trail vertexes"
                (Debug.log "All Vertex Candidates" vertexCandidates
                    |> List.filter (\v -> List.length v.c == 3)
                    |> (\vxs -> Debug.log "3c vertexes" vxs)
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


nextLinePoint : (Vertex -> Int) -> Vertex -> Vertex -> Bool
nextLinePoint axisFn cVertex pVertex =
    axisFn cVertex - axisFn pVertex == 1


pointPartition : (( Int, Int ) -> Int) -> (( Int, Int ) -> Int) -> List ( Int, Int ) -> List (List ( Int, Int ))
pointPartition partFn sortFn points =
    points
        |> List.foldl
            (\point ( mpp, part, partitions ) ->
                case mpp of
                    Nothing ->
                        ( Just point, [ point ], partitions )

                    Just pp ->
                        if partFn pp == partFn point then
                            ( Just point, point :: part, partitions )
                        else
                            ( Just point, [ point ], part :: partitions )
            )
            ( Nothing, [], [] )
        |> (\( _, lastpart, partitions ) -> lastpart :: partitions)
        |> List.map (\part -> List.sortBy (\pnt -> sortFn pnt) part)


partitionLines : (Vertex -> Int) -> List ( Int, Int ) -> List ( Vertex, Vertex )
partitionLines axisFn points =
    points
        |> List.foldl
            (\( x, y ) ( mline, partitions ) ->
                case mline of
                    ( Nothing, Nothing ) ->
                        ( ( Just (Vertex x y []), Nothing ), partitions )

                    ( Nothing, Just _ ) ->
                        ( mline, partitions )

                    ( Just v1, Nothing ) ->
                        if nextLinePoint axisFn (Vertex x y []) v1 then
                            ( ( Just v1, Just (Vertex x y []) ), partitions )
                        else
                            ( ( Just (Vertex x y []), Nothing ), partitions )

                    ( Just v1, Just v2 ) ->
                        if nextLinePoint axisFn (Vertex x y []) v2 then
                            ( ( Just v1, Just (Vertex x y []) ), partitions )
                        else
                            ( ( Just (Vertex x y []), Nothing ), ( v1, v2 ) :: partitions )
            )
            ( ( Nothing, Nothing ), [] )
        |> (\( ( mv1, mv2 ), partitions ) ->
                Maybe.map2 (\v1 v2 -> ( v1, v2 ) :: partitions)
                    mv1
                    mv2
                    |> Maybe.withDefault partitions
           )


{-| @private
-}
findLines : Set ( Int, Int ) -> List ( Vertex, Vertex )
findLines outline =
    let
        horizontal =
            outline
                |> Set.toList
                |> List.sortBy (\( _, y ) -> y)
                |> pointPartition Tuple.second Tuple.first
                |> List.map (partitionLines .x)
                |> List.concat
                |> (\result ->
                        let
                            _ =
                                Debug.log "H lines: " (List.length result)
                        in
                        result
                   )

        vertical =
            outline
                |> Set.toList
                |> List.sortBy (\( x, _ ) -> x)
                |> pointPartition Tuple.first Tuple.second
                |> List.map (partitionLines .y)
                |> List.concat
                |> (\result ->
                        let
                            _ =
                                Debug.log "V lines: " (List.length result)
                        in
                        result
                   )
    in
    horizontal ++ vertical


{-| @private
Construct lines, filter out horizontal lines, order line vertexes by y axis.
-}
findVerticalLines : List Vertex -> List ( Vertex, Vertex )
findVerticalLines vertexes =
    let
        connector vs lines =
            case vs of
                [] ->
                    lines

                v1 :: next ->
                    case next of
                        [] ->
                            lines

                        v2 :: rest ->
                            connector next (( v1, v2 ) :: lines)
    in
    List.head vertexes
        |> Maybe.map
            (\h ->
                connector (vertexes ++ [ h ]) []
                    |> List.filter (\( v1, v2 ) -> v1.x == v2.x)
                    |> List.map
                        (\( v1, v2 ) ->
                            case v1.y <= v2.y of
                                False ->
                                    ( v2, v1 )

                                True ->
                                    ( v1, v2 )
                        )
            )
        |> Maybe.withDefault []


{-| @private
Given 2 polygons as its vertical lines, check if scanpoints contain balls.
If no balls are found in scanpoints return the polygon as scanpoints dictionary,
otherwise return an empty dictionary.
-}
findPolygonsToFill : ( List ( Vertex, Vertex ), List ( Vertex, Vertex ) ) -> List (Maybe Cell) -> ( Dict ( Int, Int ) (Maybe Cell), Dict ( Int, Int ) (Maybe Cell) )
findPolygonsToFill ( polyLines1, polyLines2 ) balls =
    let
        polyArea lines =
            balls
                |> List.map
                    (\mc ->
                        mc
                            |> Maybe.map (\c -> ( ( c.cellX, c.cellY ), True ))
                            |> Maybe.withDefault ( ( 0, 0 ), False )
                    )
                |> List.filter (\( _, flag ) -> flag)
                |> Dict.fromList
                |> Dict.foldl
                    (\ball _ points ->
                        if Dict.isEmpty points then
                            Dict.empty
                        else
                            points
                                |> Dict.get ball
                                |> Maybe.map (\_ -> Dict.empty)
                                |> Maybe.withDefault points
                    )
                    (polyPoints lines Dict.empty)
    in
    ( polyArea polyLines1
    , polyArea polyLines2
    )


{-| @private
-}
fillPolygons : ( Dict ( Int, Int ) (Maybe Cell), Dict ( Int, Int ) (Maybe Cell) ) -> Grid -> Dict ( Int, Int ) (Maybe Cell)
fillPolygons ( polyArea1, polyArea2 ) grid =
    let
        fillCells area cells =
            area
                |> Dict.foldl
                    (\( x, y ) _ acc ->
                        acc
                            |> Dict.insert ( x, y ) (Cell.conquest grid.colors.conquest x y |> Just)
                    )
                    cells
    in
    grid.cells
        |> fillCells polyArea1
        |> fillCells polyArea2


{-| @private
-}
polyPoints : List ( Vertex, Vertex ) -> Dict ( Int, Int ) (Maybe Cell) -> Dict ( Int, Int ) (Maybe Cell)
polyPoints lines points =
    case lines of
        [] ->
            points

        leftLine :: restlines ->
            case restlines of
                [] ->
                    points

                rightLine :: rest ->
                    scanlinePoints leftLine rightLine points
                        |> polyPoints rest


{-| @private
-}
scanlinePoints : ( Vertex, Vertex ) -> ( Vertex, Vertex ) -> Dict ( Int, Int ) (Maybe Cell) -> Dict ( Int, Int ) (Maybe Cell)
scanlinePoints ( vl1, vl2 ) ( vr, _ ) points =
    Array.initialize (vr.x - vl1.x - 1) (\n -> vl1.x + n + 1)
        |> Array.toList
        |> List.foldl
            (\x acc ->
                Array.initialize (vl2.y - vl1.y - 1) (\n -> vl1.y + n + 1)
                    |> Array.toList
                    |> List.foldl
                        (\y xy ->
                            xy
                                |> Dict.insert ( x, y ) Nothing
                        )
                        acc
            )
            points


{-| @private
Go from a `List VertexLine` e.g. [North,South,East] to sorted "-East-North-South"
-}
directionString : List VertexConnection -> String
directionString directions =
    directions
        |> List.map Basics.toString
        |> List.sort
        |> List.foldl (\ds a -> String.append a ("-" ++ ds)) ""


{-| @private
-}
directionPredicate : VertexConnection -> Vertex -> Vertex -> Bool
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
directionSorter : VertexConnection -> Vertex -> Vertex -> Order
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
directionClockwise : Vertex -> VertexConnection
directionClockwise v =
    case directionString v.c of
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
directionTrail : Vertex -> VertexConnection
directionTrail v =
    case directionString v.c of
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
nextDirection : ( Vertex, VertexConnection ) -> Maybe VertexConnection
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
    vertex.c
        |> List.filter (\d -> d /= arraival)
        |> List.head


{-| @private
Given a vertex point and direction, trace to the next vertex point and arraival direction.
-}
nextVertex : List Vertex -> Vertex -> VertexConnection -> Maybe ( Vertex, VertexConnection )
nextVertex vertexes start direction =
    vertexes
        |> List.filter (directionPredicate direction start)
        |> List.sortWith (directionSorter direction)
        |> List.head
        |> Maybe.map (\v -> ( v, direction ))


{-| @private
-}
tracePath : List Vertex -> List Vertex -> Vertex -> Maybe ( Vertex, VertexConnection ) -> List Vertex
tracePath vertexes accum crossing mvd =
    case mvd of
        Nothing ->
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
            --grid.size.width // 2
            0

        py =
            --grid.size.height - 1
            0

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
            Debug.log "PriorPlayer" grid.priorPlayer

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
                if Cell.isSpace priorCell then
                    ( Cell.trail grid.colors.trail cx cy |> Just
                    , ( cx, cy, keyName ) :: grid.trail
                    )
                else if not <| List.isEmpty grid.trail && (Cell.isBorder priorCell || Cell.isConquest priorCell) then
                    ( Cell.trail grid.colors.trail cx cy |> Just
                    , ( px, py, keyName ) :: ( cx, cy, keyName ) :: grid.trail
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
                , trail = Debug.log "Trail" updatedTrail
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
