module GridFormat exposing (CellGrid, grid_to_list_of_stats, index_cell_type, px, py, standard_grid, standard_grid_string, string_to_grid)

import Array


type CellType
    = Blocker
    | EarthWall
    | StoneWall
    | EarthFloor
    | StoneFloor
    | Void


index_cell_type : CellType -> Int
index_cell_type c =
    case c of
        Blocker ->
            0

        StoneWall ->
            1

        EarthWall ->
            2

        StoneFloor ->
            3

        EarthFloor ->
            4

        Void ->
            5


type alias LocalCell =
    { cell_type : CellType
    , row_ix : Int
    , col_ix : Int
    }


type alias CellGrid =
    { cells : List (List LocalCell)
    , row_count : Int
    , col_count : Int
    }


grid_to_list_of_stats : CellGrid -> List ( Int, Int, CellType )
grid_to_list_of_stats grid =
    grid.cells |> List.concat |> List.map (\c -> ( c.row_ix, c.col_ix, c.cell_type ))


px : LocalCell -> Int
px c =
    c.row_ix


py : LocalCell -> Int
py c =
    c.col_ix


standard_grid : CellGrid
standard_grid =
    string_to_grid "7;7;_______0_....._0_....._0_....._0_....._0_....._0_______0"


standard_grid_string : String
standard_grid_string =
    grid_to_string standard_grid


is_valid_char : Char -> Bool
is_valid_char c =
    String.toList "_Xxo. " |> List.member c


char_to_type : Char -> CellType
char_to_type c =
    case c of
        '_' ->
            Blocker

        'X' ->
            StoneWall

        'x' ->
            EarthWall

        'o' ->
            StoneFloor

        '.' ->
            EarthFloor

        ' ' ->
            Void

        _ ->
            Void


type_to_char : CellType -> Char
type_to_char c =
    case c of
        Blocker ->
            '_'

        StoneWall ->
            'X'

        EarthWall ->
            'x'

        StoneFloor ->
            'o'

        EarthFloor ->
            '.'

        Void ->
            ' '


take_often : Int -> List a -> List (List a)
take_often n l =
    if List.length l < n then
        []

    else
        List.take n l :: take_often n (List.drop n l)


tuple_to_local_cell : ( Int, Int, CellType ) -> LocalCell
tuple_to_local_cell ( i, j, c ) =
    LocalCell c i j


zip : List a -> List b -> List ( a, b )
zip l r =
    List.map2 (\i j -> ( i, j )) l r


localise_listed_cells : List (List CellType) -> List (List LocalCell)
localise_listed_cells ll =
    let
        row_numbers =
            List.range 1 (List.length ll)

        standard_row =
            Array.fromList ll |> Array.get 0 |> Maybe.withDefault []

        col_numbers =
            List.range 1 (List.length standard_row)

        j_ed_rows =
            List.map (zip col_numbers) ll

        ij_ed_rows =
            List.map2 (\i j_row -> List.map (\( j, x ) -> tuple_to_local_cell ( i, j, x )) j_row) row_numbers j_ed_rows
    in
    ij_ed_rows


listed_local_cells_to_grid : List (List LocalCell) -> CellGrid
listed_local_cells_to_grid l =
    let
        min_row_length =
            List.map List.length l |> List.minimum |> Maybe.withDefault 0

        --min_col_length = List.length l
        ll =
            List.map (List.take min_row_length) l
    in
    { cells = ll, row_count = min_row_length, col_count = List.length l }


string_to_grid : String -> CellGrid
string_to_grid raw_s =
    let
        s_parts =
            String.split ";" raw_s |> Array.fromList

        row_count =
            Array.get 0 s_parts |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0

        col_count =
            Array.get 1 s_parts |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0

        s =
            Array.get 2 s_parts |> Maybe.withDefault ""

        fully_typed_grid =
            String.toList s
                |> List.filter is_valid_char
                |> List.map char_to_type
                |> take_often col_count
                |> List.take row_count
                |> localise_listed_cells
                |> listed_local_cells_to_grid
    in
    fully_typed_grid


grid_to_string : CellGrid -> String
grid_to_string cell_grid =
    let
        row_count =
            List.length cell_grid.cells |> String.fromInt

        col_count =
            List.head cell_grid.cells |> Maybe.withDefault [] |> List.length |> String.fromInt

        s =
            List.concat cell_grid.cells |> List.map .cell_type |> List.map type_to_char |> String.fromList
    in
    row_count ++ ";" ++ col_count ++ ";" ++ s
