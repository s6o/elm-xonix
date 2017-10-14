module View
    exposing
        ( view
        )

import Cell exposing (Cell, Shape(..))
import Collage as C exposing (Form)
import Dict
import Element as E
import Grid exposing (Grid)
import Html exposing (Html, text)
import Html.Lazy exposing (lazy2)
import Model exposing (Model, Msg(..))
import Window as W


view : Model -> Html Msg
view model =
    model.wsize
        |> Maybe.map (\ws -> lazy2 render ws model.grid)
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
        |> E.container ws.width ws.height E.middle
        |> E.toHtml


renderGrid : Grid -> Form
renderGrid g =
    g.cells
        |> Dict.values
        |> List.map (renderCell g)
        |> C.group


renderCell : Grid -> Cell -> Form
renderCell g c =
    case c.shape of
        Ball _ ->
            Cell.size // 2
                |> toFloat
                |> C.circle
                |> C.filled c.color
                |> C.move (canvasXY g c)

        Border ->
            renderBlock g c

        Player ->
            renderBlock g c

        Space ->
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



{- }
   render : Model -> Html Msg
   render m =
     let
       cs = m.grid.config.cellSize
       pw = (m.grid.config.gridWidth * m.grid.config.cellSize)
       ph = (m.grid.config.gridHeight * m.grid.config.cellSize)
       toCoords c =
         ( ((pw // 2) - pw) + (c.cx * cs) + (cs // 2) |> toFloat
         , ph - (ph // 2) - (c.cy * cs) - (cs // 2) |> toFloat
         )
     in
       m.wsize
         |> Maybe.map (\ws ->
           Dict.values m.grid.cells
             |> List.map (\c ->
               case c.shape of
                 Rectangle ->
                   toFloat cs
                     |> C.square
                     |> C.filled c.color
                     |> C.move (toCoords c)
                 Circle ->
                   toFloat cs / 2
                     |> C.circle
                     |> C.filled c.color
                     |> C.move (toCoords c)
             )
             |> C.collage pw ph
             |> E.container ws.width ws.height E.middle
             |> E.toHtml
         )
         |> Maybe.withDefault (text "Waiting for Window size ...")
-}
