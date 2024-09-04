port module Main exposing (main)

import Browser
import Browser.Events
import Css
import Css.Global exposing (body, global, html)
import TileGrid exposing (TileGrid, add_row, add_column, remove_row, remove_column, grid_to_list_of_stats, standard_grid, grid_to_string)
import TileTheme exposing (cell_colour)
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Svg.Styled
import Svg.Styled.Attributes
import Platform.Cmd as Cmd



port sendMessage : String -> Cmd msg

type alias Model =
    { hover_colour : Css.Color, current_grid : TileGrid, svg_cell_size : Int }


type Msg
    = KeyEventLetter Char
    | KeyEventString String
    | AddRow 
    | AddCol
    | RemRow
    | RemCol 
    | LevelToClipboard


init : String -> ( Model, Cmd Msg )
init level_string =
    ( { hover_colour = Css.rgba 255 0 0 0.2, current_grid = standard_grid level_string, svg_cell_size = 60 }
    , Cmd.none
    )


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> Browser.Document "🫖🛏️ - TeaBed, A Tile Based Editor" [view model |> Html.Styled.toUnstyled]
        , update = update
        , subscriptions = subscriptions
        }


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            KeyEventLetter char

        _ ->
            KeyEventString string


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyEventLetter char ->
            if char == 'q' then
                ( { model | hover_colour = Css.rgba 0 255 0 0.3 }, Cmd.none )

            else
                ( { model | hover_colour = Css.rgba 0 0 255 0.3 }, Cmd.none )
        AddRow -> 
            ( {model | current_grid = add_row model.current_grid}, Cmd.none)
        AddCol -> 
            ( {model | current_grid = add_column model.current_grid}, Cmd.none)
        RemRow -> 
            ( {model | current_grid = remove_row model.current_grid}, Cmd.none)
        RemCol -> 
            ( {model | current_grid = remove_column model.current_grid}, Cmd.none)
        LevelToClipboard ->
            (model, sendMessage (grid_to_string model.current_grid))
        _ ->
            ( model, Cmd.none )


view : Model -> Html.Styled.Html Msg
view model =
    let
        standard_css_header =
            [ global
                [ body
                    [ Css.backgroundColor (Css.rgb 0 0 0)
                    , Css.fontFamily Css.monospace
                    , Css.fontVariant Css.smallCaps
                    , Css.fontSize (Css.vmin 2)
                    , Css.fontWeight (Css.int 352)
                    , Css.color (Css.rgb 225 225 225)
                    , Css.overflow Css.hidden
                    ]
                , html
                    [ Css.height (Css.pct 100)
                    , Css.width (Css.pct 100)
                    ]
                ]
            ]
    in
    Html.Styled.div []
        ((standard_css_header
            ++ [ Svg.Styled.svg
                    svg_frame_attributes
                    (standard_svg_grid model)
               ]
        ) ++ [
                Html.Styled.div  
                    [css [ Css.position Css.absolute, Css.top (Css.px 10), Css.right (Css.px 10), Css.width (Css.vw 10)]]
                    [Html.Styled.text "Add/Remove Rows/Colums"
                    , Html.Styled.table 
                        []
                        [ Html.Styled.tr [ css [Css.fontSize (Css.vmin 1)  ] ]
                            [Html.Styled.td [onClick AddRow, css [Css.hover [ Css.backgroundColor (Css.hex "#100")],Css.active [ Css.backgroundColor (Css.hex "#001")]]] [Html.Styled.text "R+"]
                            ,Html.Styled.td [onClick RemRow, css [Css.hover [ Css.backgroundColor (Css.hex "#100")],Css.active [ Css.backgroundColor (Css.hex "#001")]]] [Html.Styled.text "R-"]]
                    , Html.Styled.tr [ css [Css.fontSize (Css.vmin 1)  ] ]
                            [Html.Styled.td [onClick AddCol, css [Css.hover [ Css.backgroundColor (Css.hex "#100")],Css.active [ Css.backgroundColor (Css.hex "#001")]]] [Html.Styled.text "C+"]
                            ,Html.Styled.td [onClick RemCol, css [Css.hover [ Css.backgroundColor (Css.hex "#100")],Css.active [ Css.backgroundColor (Css.hex "#001")]]] [Html.Styled.text "C-"]]
                    ]
                    , Html.Styled.div
                        [ onClick LevelToClipboard, css [ Css.backgroundColor (Css.hex "#222"), Css.property "word-wrap" "break-word", Css.position Css.absolute, Css.bottom (Css.vh -50), Css.width (Css.vw 10), Css.height (Css.vh 10)] ]
                        [ Html.Styled.text "click to clipboard" ]
                    ]     
        ]           
            )


svg_frame_attributes : List (Html.Styled.Attribute msg)
svg_frame_attributes =
    [ css
        [ Css.backgroundColor (Css.rgba 0 0 0 0)
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.overflow Css.hidden
        ]
    , Svg.Styled.Attributes.width "100vw"
    , Svg.Styled.Attributes.height "100vh"
    , Svg.Styled.Attributes.viewBox "0 0 3840 2160"
    ]



standard_svg_grid : Model -> List (Svg.Styled.Svg msg)
standard_svg_grid model =
    let
        r i j clr =
            Svg.Styled.rect
                [ Svg.Styled.Attributes.x (String.fromInt i)
                , Svg.Styled.Attributes.y (String.fromInt j)
                , Svg.Styled.Attributes.width (String.fromInt model.svg_cell_size)
                , Svg.Styled.Attributes.height (String.fromInt model.svg_cell_size)
                , Svg.Styled.Attributes.fill clr
                , Svg.Styled.Attributes.strokeWidth "0.5"
                , Svg.Styled.Attributes.stroke "#333"
                , css
                    [ Css.hover [ Css.fill model.hover_colour ], 
                      Css.active [ Css.fill (Css.hex "#001")]]
                ]
                []

        list_of_cells =
            grid_to_list_of_stats model.current_grid |> List.map (\( a, b, c ) -> ( a, b, cell_colour c ))

        cells =
            List.map
                (\( j, i, clr ) -> r (model.svg_cell_size * i) (model.svg_cell_size * j) clr)
                list_of_cells
    in
    cells
