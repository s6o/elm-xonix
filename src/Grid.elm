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


type alias Turns =
    { count : Int
    , directions : List VertexConnection
    }


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


{-| Realized that, the border needs to shift to were the (new) trail was cut.
The old (outer most) border will become part of conquered space.

Thus, first conquerSpace:

  - find outline points
  - find lines from outline points
  - find vertexes from lines
  - find the 2 polygons via vertexes (and lines)
  - check which of the 2 polygons can be filled (contain no balls)
  - fill (conquer) polygons without balls - in fill, remember to exclude trail
  - fill trail to be the new border

-}
conquer : Grid -> Grid
conquer grid =
    --conquerTrail grid
    conquerSpace grid


conquerSpace : Grid -> Grid
conquerSpace grid =
    let
        vertexes =
            Debug.log "vertexes" (findVertexes grid)

        lines =
            Debug.log "lines" (findLines vertexes)

        ( poly1, poly2 ) =
            Debug.log "polygons" findPolyVertexes grid vertexes

        ( polyLines1, polyLines2 ) =
            ( Debug.log "poly 1 lines" (findPolyLines poly1 lines)
            , Debug.log "poly 2 lines" (findPolyLines poly2 lines)
            )

        ( polyPoints1, polyPoints2 ) =
            ( Debug.log "poly 1 points" (findPolyPoints grid poly1 polyLines1)
            , Debug.log "poly 2 points" (findPolyPoints grid poly2 polyLines2)
            )

        {-
           polysToFill =
               findPolygonsToFill polyLines grid.balls
        -}
    in
    grid



{-
      |> (\g -> { g | cells = debugOutline outline g |> debugVertexes vertexes })
      |> (\g -> { g | cells = g.cells |> Dict.map (\( x, y ) mc -> mc |> Maybe.map (\c -> { c | color = Color.darkGrey })) })
   |> (\g -> { g | cells = fillPolygons polysToFill g, trail = [] })
-}


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
-}
countTurns : Dict ( Int, Int ) (Maybe Cell) -> ( Int, Int ) -> Turns
countTurns cells point =
    let
        turnPoints ( x, y ) =
            [ { p1 = ( x + 1, y )
              , p2 = ( x, y + 1 )
              , space = ( x + 1, y + 1 )
              , dirs = [ East, South ]
              }
            , { p1 = ( x, y + 1 )
              , p2 = ( x - 1, y )
              , space = ( x - 1, y + 1 )
              , dirs = [ South, West ]
              }
            , { p1 = ( x - 1, y )
              , p2 = ( x, y - 1 )
              , space = ( x - 1, y - 1 )
              , dirs = [ North, West ]
              }
            , { p1 = ( x, y - 1 )
              , p2 = ( x + 1, y )
              , space = ( x + 1, y - 1 )
              , dirs = [ East, North ]
              }
            ]
    in
    turnPoints point
        |> List.foldl
            (\{ p1, p2, space, dirs } accum ->
                case ( Dict.get p1 cells, Dict.get p2 cells, Dict.get space cells ) of
                    ( Just mc1, Just mc2, Just mcs ) ->
                        if (Cell.isBorder mc1 || Cell.isTrail mc1) && (Cell.isBorder mc2 || Cell.isTrail mc2) && Cell.isSpace mcs then
                            { accum
                                | count = accum.count + 1
                                , directions =
                                    dirs
                                        |> List.foldl
                                            (\dir a ->
                                                a
                                                    |> List.filter (\d -> d == dir)
                                                    |> List.head
                                                    |> Maybe.map (\_ -> a)
                                                    |> Maybe.withDefault (dir :: a)
                                            )
                                            accum.directions
                            }
                        else
                            accum

                    _ ->
                        accum
            )
            { count = 0, directions = [] }


findVertexes : Grid -> List Vertex
findVertexes grid =
    grid.cells
        |> Dict.filter (\_ mc -> Cell.isPlayer mc || Cell.isBorder mc || Cell.isTrail mc)
        |> Dict.foldl
            (\( cx, cy ) _ accum ->
                let
                    { count, directions } =
                        countTurns grid.cells ( cx, cy )
                in
                if count >= 1 then
                    { x = cx, y = cy, c = directions } :: accum
                else
                    accum
            )
            []


{-| @private

A trail left by the player, dividing the empty area will always create 2 polygons.
There are 2 vertexes, at each end of the trail, that have 3 line connections,
while the rest have two connections.

      * 1) find the 2 trail vertexes
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
findPolyVertexes : Grid -> List Vertex -> ( List Vertex, List Vertex )
findPolyVertexes grid vertexes =
    let
        findTrailVertex ( x, y, _ ) =
            vertexes
                |> List.filter (\v -> v.x == x && v.y == y && List.length v.c == 3)
                |> List.head
                |> Maybe.map Basics.identity

        ( trailv1, trailv2 ) =
            ( grid.trail
                |> List.head
                |> Maybe.map findTrailVertex
                |> EMaybe.join
            , grid.trail
                |> List.reverse
                |> List.head
                |> Maybe.map findTrailVertex
                |> EMaybe.join
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


vertexPairs : (Vertex -> Int) -> List ( Vertex, Vertex ) -> List Vertex -> List ( Vertex, Vertex )
vertexPairs sortAxisFn accum vertexes =
    case vertexes of
        [] ->
            accum

        v :: [] ->
            accum

        v1 :: v2 :: [] ->
            if sortAxisFn v1 == sortAxisFn v2 then
                ( v1, v2 ) :: accum
            else
                accum

        v1 :: v2 :: rest ->
            let
                nextAccum =
                    if sortAxisFn v1 == sortAxisFn v2 then
                        ( v1, v2 ) :: accum
                    else
                        accum
            in
            vertexPairs sortAxisFn nextAccum (v2 :: rest)


orderByX : Vertex -> Vertex -> Order
orderByX vl vr =
    if vl.x > vr.x then
        GT
    else if vl.x < vr.x then
        LT
    else if vl.y == vr.y then
        EQ
    else if vl.y > vr.y then
        GT
    else
        LT


orderByY : Vertex -> Vertex -> Order
orderByY vl vr =
    if vl.y > vr.y then
        GT
    else if vl.y < vr.y then
        LT
    else if vl.x == vr.x then
        EQ
    else if vl.x > vr.x then
        GT
    else
        LT


{-| @private
-}
findLines : List Vertex -> List ( Vertex, Vertex )
findLines vertexes =
    let
        horizontal =
            vertexes
                |> List.sortWith orderByY
                |> vertexPairs .y []
                |> (\result ->
                        let
                            _ =
                                Debug.log "H lines: " (List.length result)
                        in
                        result
                   )

        vertical =
            vertexes
                |> List.sortWith orderByX
                |> vertexPairs .x []
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
Given a list of polygon vertexes and total lines, filter out polygon lines.
-}
findPolyLines : List Vertex -> List ( Vertex, Vertex ) -> List ( Vertex, Vertex )
findPolyLines vertexes lines =
    let
        isMember v =
            vertexes
                |> List.filter (\vx -> vx.x == v.x && vx.y == v.y && vx.c == v.c)
                |> List.head
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False
    in
    lines
        |> List.filter (\( v1, v2 ) -> isMember v1 && isMember v2)
        |> (\l ->
                let
                    _ =
                        Debug.log "Poly line count" (List.length l)
                in
                l
           )


{-| @private
Given trail, polygon vertexes and edges, find all points inside the polygon.

  - the search area for points is the surrounding bounding box for the polygon
  - return empty list if 'search space' contains balls
  - eliminate Trail points from the final result, as trail will be the new border
    and not part of the conquered space
  - ray casting based on <http://philliplemons.com/posts/ray-casting-algorithm>

-}
findPolyPoints : Grid -> List Vertex -> List ( Vertex, Vertex ) -> List ( Int, Int )
findPolyPoints grid polyVertexes edges =
    let
        trailPoints =
            grid.trail
                |> List.map (\( x, y, _ ) -> ( x, y ))

        ( xmin, xmax ) =
            polyVertexes
                |> List.sortWith orderByX
                |> List.map .x
                |> (\l -> ( List.minimum l |> Maybe.withDefault 0, List.maximum l |> Maybe.withDefault 0 ))
                |> Debug.log "xmin, xmax"

        ( ymin, ymax ) =
            polyVertexes
                |> List.sortWith orderByY
                |> List.map .y
                |> (\l -> ( List.minimum l |> Maybe.withDefault 0, List.maximum l |> Maybe.withDefault 0 ))
                |> Debug.log "ymin, ymax"

        xbounds =
            Array.initialize (xmax - xmin + 1) identity
                |> Array.toList
                |> Debug.log "xbounds"

        ybounds =
            Array.initialize (ymax - ymin + 1) identity
                |> Array.toList
                |> Debug.log "ybounds"

        edgeAB ( v1, v2 ) =
            if v1.y <= v2.y then
                ( v1, v2 )
            else
                ( v2, v1 )

        isPolyPoint ( x, y ) =
            edges
                |> List.foldl
                    (\e inside ->
                        let
                            ( a, b ) =
                                edgeAB e
                        in
                        if y == a.y || y == b.y then
                            not inside
                        else if y > b.y || y < a.y || x > Basics.max a.x b.x then
                            inside
                        else if x < Basics.min a.x b.x then
                            not inside
                        else
                            inside
                    )
                    False

        searchSpace =
            xbounds
                |> List.foldl
                    (\xbi accum ->
                        ybounds
                            |> List.foldl (\ybi a -> ( xmin + xbi, ymin + ybi ) :: a) accum
                    )
                    []

        selectSearchSpace =
            grid.balls
                |> List.foldl
                    (\mc inside ->
                        mc
                            |> Maybe.map (\c -> List.member ( c.cellX, c.cellY ) searchSpace)
                            |> Maybe.withDefault False
                    )
                    False
                |> (\ballsFound ->
                        if ballsFound then
                            []
                        else
                            searchSpace
                   )
    in
    selectSearchSpace
        |> List.foldl
            (\point accum ->
                case isPolyPoint point of
                    False ->
                        accum

                    True ->
                        point :: accum
            )
            []
        |> List.filter (\p -> List.member p trailPoints |> not)


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
directionClockwise : Maybe Vertex -> VertexConnection
directionClockwise mv =
    case mv of
        Nothing ->
            Debug.crash "Missing Trail vertex"

        Just v ->
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
directionTrail : Maybe Vertex -> VertexConnection
directionTrail mv =
    case mv of
        Nothing ->
            Debug.crash "Missing Trail vertex"

        Just v ->
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
nextVertex : List Vertex -> Maybe Vertex -> VertexConnection -> Maybe ( Vertex, VertexConnection )
nextVertex vertexes mv direction =
    case mv of
        Nothing ->
            Debug.crash "Missing Trail vertex"

        Just start ->
            vertexes
                |> List.filter (directionPredicate direction start)
                |> List.sortWith (directionSorter direction)
                |> List.head
                |> Maybe.map (\v -> ( v, direction ))


{-| @private
-}
tracePath : List Vertex -> List Vertex -> Maybe Vertex -> Maybe ( Vertex, VertexConnection ) -> List Vertex
tracePath vertexes accum mv mvd =
    case mv of
        Nothing ->
            Debug.crash "Missing Trail vertex"

        Just crossing ->
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
                                        |> Maybe.map (nextVertex vertexes (Just current))
                                        |> EMaybe.join
                            in
                            tracePath vertexes (current :: accum) (Just crossing) next


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
