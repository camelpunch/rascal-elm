module View exposing (view)

import Html exposing (Html, text, div, table, tr, td)
import Model exposing (Model)


type Piece
    = Player
    | Brick
    | EmptySpace


view : Model -> Html msg
view model =
    let
        width =
            model.board.x
    in
        table []
            (List.map (row model) [0..width])


brick : Int -> Html msg
brick x =
    td []
        [ text (renderPiece Brick) ]


row : Model -> Int -> Html msg
row model y =
    let
        columnCount =
            model.board.y
    in
        tr []
            (List.map (\x -> cell x y model) [0..columnCount])


cell : Int -> Int -> Model -> Html msg
cell x y model =
    let
        char =
            if x == 0 || x == model.board.x || y == 0 || y == model.board.y then
                renderPiece Brick
            else if model.player.x == x && model.player.y == y then
                renderPiece Player
            else
                renderPiece EmptySpace
    in
        td []
            [ text char ]


renderPiece : Piece -> String
renderPiece piece =
    case piece of
        Player ->
            "@"

        Brick ->
            "#"

        EmptySpace ->
            "."
