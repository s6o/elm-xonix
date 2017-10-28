module Grid
    exposing
        ( Colors
        , Grid
        , Size
        , animate
        , clearTrail
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
    , trail : Set ( Int, Int )
    , trailLeft : Set ( Int, Int )
    , trailRight : Set ( Int, Int )
    }


{-| Main game object colors.
-}
type alias Colors =
    { ball : Color
    , border : Color
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


animate : Time -> Grid -> Grid
animate systemTick grid =
    { grid
        | animationDelta =
            grid.animation
                |> Maybe.map (\a -> Animation.animate systemTick a)
                |> Maybe.withDefault 0
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
                |> Set.foldl
                    (\( x, y ) accum -> Dict.insert ( x, y ) Nothing accum)
                    grid.cells
                |> (\cells -> Dict.insert ( px, py ) Nothing cells)
        , trail = Set.empty
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
        , player = Color.darkRed
        , space = Color.white
        , trail = Color.darkYellow
        }
    , priorPlayer = ( ( -1, -1 ), Nothing )
    , size = { height = 46, width = 64 }
    , trail = Set.empty
    , trailLeft = Set.empty
    , trailRight = Set.empty
    }


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
                    , Set.insert ( cx, cy ) grid.trail
                    )
                else
                    ( pc
                    , grid.trail
                    )
        in
        if Cell.isTrail priorCell || (Cell.isSpace pc && Set.isEmpty grid.trail) then
            ( grid
            , Task.succeed TakeLife
                |> Task.perform identity
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
            , Cmd.none
            )
    else
        ( grid
        , Cmd.none
        )


nextBallPositions : Grid -> List (Maybe Cell)
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
                            c
                )
            )


{-| @private
-}
nextBallNE : Grid -> Cell -> Cell
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
    if Cell.isSpace nextCell then
        { c | cellX = c.cellX + 1, cellY = c.cellY - 1 }
    else if Cell.isSpace leftCell then
        { c | cellX = c.cellX - 1, cellY = c.cellY - 1, shape = Ball NW }
    else if Cell.isSpace rightCell then
        { c | cellX = c.cellX + 1, cellY = c.cellY + 1, shape = Ball SE }
    else
        { c | cellX = c.cellX - 1, cellY = c.cellY + 1, shape = Ball SW }


{-| @private
-}
nextBallNW : Grid -> Cell -> Cell
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
    if Cell.isSpace nextCell then
        { c | cellX = c.cellX - 1, cellY = c.cellY - 1 }
    else if Cell.isSpace leftCell then
        { c | cellX = c.cellX - 1, cellY = c.cellY + 1, shape = Ball SW }
    else if Cell.isSpace rightCell then
        { c | cellX = c.cellX + 1, cellY = c.cellY - 1, shape = Ball NE }
    else
        { c | cellX = c.cellX + 1, cellY = c.cellY + 1, shape = Ball SE }


{-| @private
-}
nextBallSE : Grid -> Cell -> Cell
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
    if Cell.isSpace nextCell then
        { c | cellX = c.cellX + 1, cellY = c.cellY + 1 }
    else if Cell.isSpace leftCell then
        { c | cellX = c.cellX + 1, cellY = c.cellY - 1, shape = Ball NE }
    else if Cell.isSpace rightCell then
        { c | cellX = c.cellX - 1, cellY = c.cellY + 1, shape = Ball SW }
    else
        { c | cellX = c.cellX - 1, cellY = c.cellY - 1, shape = Ball NW }


{-| @private
-}
nextBallSW : Grid -> Cell -> Cell
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
    if Cell.isSpace nextCell then
        { c | cellX = c.cellX - 1, cellY = c.cellY + 1 }
    else if Cell.isSpace leftCell then
        { c | cellX = c.cellX + 1, cellY = c.cellY + 1, shape = Ball SE }
    else if Cell.isSpace rightCell then
        { c | cellX = c.cellX - 1, cellY = c.cellY - 1, shape = Ball NW }
    else
        { c | cellX = c.cellX + 1, cellY = c.cellY - 1, shape = Ball NE }


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
