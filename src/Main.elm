module Main exposing (main)

import Browser
import Browser.Events
import Css
import Css.Global exposing (body, global, html)
import GridFormat exposing (CellGrid, grid_to_list_of_stats, index_cell_type, px, py, standard_grid)
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Json.Decode as Decode
import Svg.Styled
import Svg.Styled.Attributes


type alias Model =
    { hover_colour : Css.Color, current_grid : CellGrid, svg_cell_size : Int }


type Msg
    = KeyEventLetter Char
    | KeyEventString String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hover_colour = Css.rgba 255 0 0 0.2, current_grid = standard_grid, svg_cell_size = 60 }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = \model -> view model |> Html.Styled.toUnstyled
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
                    , Css.fontSize (Css.vmin 4)
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
        (standard_css_header
            ++ [ Svg.Styled.svg
                    svg_frame_attributes
                    (standard_svg_grid model)
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


draft_colour : Int -> String
draft_colour i =
    case i of
        0 ->
            "#000"

        1 ->
            "#222"

        2 ->
            "#210"

        3 ->
            "#111"

        4 ->
            "#100500"

        5 ->
            "#0000"

        _ ->
            "#0000"


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
                , Svg.Styled.Attributes.strokeWidth "0"
                , Svg.Styled.Attributes.stroke "#000"
                , css
                    [ Css.hover [ Css.fill model.hover_colour ] ]
                ]
                []

        list_of_cells =
            grid_to_list_of_stats model.current_grid |> List.map (\( a, b, c ) -> ( a, b, index_cell_type c |> draft_colour ))

        cells =
            List.map
                (\( i, j, clr ) -> r (model.svg_cell_size * i) (model.svg_cell_size * j) clr)
                list_of_cells
    in
    cells
