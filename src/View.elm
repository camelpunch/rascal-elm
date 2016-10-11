module View exposing (view)

import Html exposing (Html, text, div, table, tr, td)
import Html.CssHelpers
import Model exposing (Model, Point, Piece(..), cell)
import SharedStyles


{ id, class, classList } =
    Html.CssHelpers.withNamespace SharedStyles.rascalNamespace
view : Model -> Html msg
view model =
    let
        ( width, height ) =
            model.board
    in
        table [ id SharedStyles.Page ]
            (List.map (row model) [0..height])


brick : Int -> Html msg
brick x =
    td []
        [ text (renderPiece Brick) ]


row : Model -> Int -> Html msg
row model y =
    let
        ( width, height ) =
            model.board
    in
        tr []
            (List.map (\x -> td [] [ text (renderPiece (cell { x = x, y = y } model)) ]) [0..width])


renderPiece : Piece -> String
renderPiece piece =
    case piece of
        Player ->
            "@"

        Brick ->
            "#"

        EmptySpace ->
            "."
