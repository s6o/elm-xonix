module Grid
    exposing
        ( Grid
        , Colors
        , Size
        , init
        , update
        )

import Cell exposing (Cell, Direction(..), Shape(..))
import Color exposing (Color)
import Dict exposing (Dict)
import Random exposing (Generator, Seed)
import Task exposing (Task)
import Time


{-| `Grid` is about capturing all things required for the renderer to visualize
the next game state.

The game grid coordinate (0,0) is top/left corner, both axis contain only
positive values.
-}
type alias Grid =
    { balls : List ( Int, Int )
    , cells : Dict ( Int, Int ) Cell
    , colors : Colors
    , player : Maybe ( Int, Int )
    , size : Size
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


{-| @private
Create an empty grid with specified size: height/width
-}
empty : Grid
empty =
    { balls = []
    , cells = Dict.empty
    , colors =
        { ball = Color.orange
        , border = Color.lightBlue
        , player = Color.darkRed
        , space = Color.white
        , trail = Color.darkYellow
        }
    , player = Nothing
    , size = { height = 46, width = 64 }
    }


{-| Initialize game grid for a given level.
-}
init : Int -> (( List ( Int, Int ), Seed ) -> msg) -> ( Grid, Cmd msg )
init level posMsg =
    let
        grid =
            empty
                |> space
                |> xBorder 0
                |> (\g -> xBorder (g.size.height - 1) g)
                |> yBorder 0
                |> (\g -> yBorder (g.size.width - 1) g)
    in
        ( grid
        , initBalls level grid
            |> Task.perform posMsg
        )


{-| Partition and update `Cell`s in `Shape` clusters.
-}
update : Grid -> List Cell -> Grid
update grid cells =
    [ Ball NE
    , Border
    , Player
    , Space
    , Trail
    ]
        |> List.map (\shape -> List.filter (\c -> Cell.equal shape c.shape) cells)
        |> List.foldl
            (\sameCells g ->
                case List.head sameCells |> Maybe.map .shape of
                    Nothing ->
                        g

                    Just (Ball direction) ->
                        shapePositions (Ball direction) g
                            |> setToShape Space g.colors.space g
                            |> setToCell sameCells

                    Just Border ->
                        g

                    Just Player ->
                        g

                    Just Space ->
                        g

                    Just Trail ->
                        g
            )
            grid


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


{-| @private
Given a count generate number of random `Grid` coordinates for placeing game objects.
-}
positionGenerator : Int -> Grid -> Generator (List ( Int, Int ))
positionGenerator count g =
    Random.list count <|
        Random.pair
            (Random.int 0 (g.size.width - 2))
            (Random.int 0 (g.size.height - 2))


{-| @private
-}
setToCell : List Cell -> Grid -> Grid
setToCell cells grid =
    cells
        |> List.foldl
            (\c cells ->
                Dict.update ( c.cellX, c.cellY ) (\_ -> Just c) cells
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
                Dict.update ( x, y ) (\_ -> Just <| Cell x y c s) cells
            )
            grid.cells
        |> (\newCells -> { grid | cells = newCells })


{-| @private
-}
shapePositions : Shape -> Grid -> List ( Int, Int )
shapePositions s g =
    g.cells
        |> Dict.filter (\_ c -> Cell.equal c.shape s)
        |> Dict.keys


{-| @private
Initialize the empty space i.e. the board area to be captured by the player while
evading collisions with the balls.
-}
space : Grid -> Grid
space g =
    List.repeat (g.size.height - 2) (Cell.space g.colors.space)
        |> List.repeat (g.size.width - 2)
        |> List.indexedMap (\x cols -> List.indexedMap (\y blockFn -> blockFn (x + 1) (y + 1)) cols)
        |> List.concat
        |> List.foldl (\c -> Dict.insert ( c.cellX, c.cellY ) c) g.cells
        |> (\newCells -> { g | cells = newCells })


{-| @private
Given an X coordinate initialize a horizontal game board's border.
-}
xBorder : Int -> Grid -> Grid
xBorder y g =
    List.repeat g.size.width (Cell.border g.colors.border)
        |> List.indexedMap (\i blockFn -> blockFn i y)
        |> List.foldl (\c -> Dict.insert ( c.cellX, c.cellY ) c) g.cells
        |> (\newCells -> { g | cells = newCells })


{-| @private
Given an Y coordinate initialize a vertical game board's border.
-}
yBorder : Int -> Grid -> Grid
yBorder x g =
    List.repeat g.size.height (Cell.border g.colors.border)
        |> List.indexedMap (\i blockFn -> blockFn x i)
        |> List.foldl (\c -> Dict.insert ( c.cellX, c.cellY ) c) g.cells
        |> (\newCells -> { g | cells = newCells })
