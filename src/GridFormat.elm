module GridFormat exposing (CellGrid, add_row, add_column, remove_row, remove_column, grid_to_list_of_stats, grid_to_string, standard_grid)

import Array
import CellType exposing (CellType, blocker_type, char_to_type, type_to_char)

add_row : CellGrid -> CellGrid
add_row grid = 
    let
        new_row_count = List.length grid.cells + 1

        new_row_types = List.repeat grid.col_count blocker_type 

        new_row = List.map2 
                        (\c i -> LocalCell c new_row_count i ) 
                        new_row_types
                        (List.range 1 
                                    ((List.length (List.head grid.cells |> Maybe.withDefault [])) + 1 )  ) 

        cells = grid.cells ++ [ new_row ]
    in    
    {cells = cells, row_count = new_row_count, col_count = grid.col_count}

add_column : CellGrid -> CellGrid
add_column grid = 
    let 
        new_col_count = ((List.length (List.head grid.cells |> Maybe.withDefault [])) + 1 )

        new_rows = List.map2 (\i row -> row ++ [LocalCell blocker_type i new_col_count] ) (List.range 1 (List.length grid.cells + 1)) grid.cells
    in 
    { cells = new_rows, row_count = grid.row_count, col_count = new_col_count}

remove_row : CellGrid -> CellGrid
remove_row grid = 
    let
        new_row_count = List.length grid.cells - 1
        cells = List.take new_row_count grid.cells
    in    
        { cells = cells, row_count = new_row_count, col_count = grid.col_count }

remove_column : CellGrid -> CellGrid
remove_column grid = 
    let 
        new_col_count = ((List.length (List.head grid.cells |> Maybe.withDefault [])) - 1 )

        new_rows = List.map (List.take new_col_count ) grid.cells
    in 
    { cells = new_rows, row_count = grid.row_count, col_count = new_col_count}


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


standard_grid : String -> CellGrid
standard_grid try_str =
    if is_level_string try_str then   
        --(Debug.log "DEBUG" try_str) |> string_to_grid
        try_str |> string_to_grid
    else 
        string_to_grid standard_grid_string

is_level_string : String -> Bool
is_level_string try_str =
    let
        pieces = String.split ";" try_str

        length_okay = ( List.length pieces == 3 )

        ppieces = List.take 3 pieces

        row_string = ppieces |> Array.fromList |> Array.get 0 |> Maybe.withDefault "7"

        col_string = ppieces |> Array.fromList |> Array.get 1 |> Maybe.withDefault "7"

        lvl_string = ppieces |> Array.fromList |> Array.get 2 |> Maybe.withDefault ""

        row_length_okay = ((String.toInt row_string ) /= Nothing )

        col_length_okay = ((String.toInt col_string ) /= Nothing )

        lvl_string_okay = (String.length lvl_string == (String.toInt col_string |> Maybe.withDefault 0)*(String.toInt row_string |> Maybe.withDefault 0))
    in    
    length_okay && row_length_okay && col_length_okay && lvl_string_okay

standard_grid_string : String
standard_grid_string =
    "7;7;_______0_....._0_....._0_....._0_....._0_....._0_______0"


is_valid_char : Char -> Bool
is_valid_char c =
    String.toList "_Xxo.+" |> List.member c



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
