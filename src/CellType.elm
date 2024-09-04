module CellType exposing (CellType, blocker_type, index_cell_type, char_to_type, type_to_char)


type CellType
    = Blocker
    | EarthWall
    | StoneWall
    | EarthFloor
    | StoneFloor
    | Void

blocker_type : CellType
blocker_type = Blocker

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

        '+' ->
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
            '+'
