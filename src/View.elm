module View exposing
  ( view
  )

import Model exposing (CellShape(..), Model, Msg(..))
import Collage as C
import Dict exposing (Dict)
import Element as E
import Html exposing (Html, text)
import Html.Lazy


view : Model -> Html Msg
view m =
  {-Html.Lazy.lazy-} render m

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