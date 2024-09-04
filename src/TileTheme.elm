module TileTheme exposing (cell_colour)

import TileType exposing (TileSymbol,index_cell_type)

cell_colour : TileSymbol -> String
cell_colour i =
    case index_cell_type i of
        0 ->
            "#111"

        1 ->
            "#444"

        2 ->
            "#530"

        3 ->
            "#333"

        4 ->
            "#210"

        5 ->
            "#0000"

        _ ->
            "#0000"