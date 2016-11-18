module View exposing (view)

import Html exposing (Html, text, div, table, tr, td, ul, li, h1)
import Html.CssHelpers
import Actor
import Model exposing (Model)
import Occupant exposing (..)
import SharedStyles exposing (..)
import Application exposing (cellOccupant)


{ id, class, classList } =
    SharedStyles.rascalNamespace


view : Model -> Html msg
view model =
    let
        ( width, height ) =
            model.board

        content =
            if Actor.isDead model.player then
                [ h1 [ class [ Central ] ] [ text "Game Over" ] ]
            else
                [ table [ class [ GameWidth ] ]
                    (List.map (row model) (List.range 0 height))
                , ul []
                    ((li []
                        [ text ("Player " ++ toString model.player.health) ]
                     )
                        :: (List.map
                                (\m ->
                                    li []
                                        [ text ("Monster " ++ toString m.health) ]
                                )
                                model.monsters
                           )
                    )
                ]
    in
        div [ id Page, class [ GameWidth ] ] content


row : Model -> Int -> Html msg
row model row =
    let
        ( width, height ) =
            model.board
    in
        tr []
            (List.map
                (\col -> column col row model)
                (List.range 0 width)
            )


column : Int -> Int -> Model -> Html msg
column col row model =
    td []
        [ text
            (renderPiece
                (cellOccupant
                    { x = col, y = row }
                    model
                )
            )
        ]


renderPiece : Occupant -> String
renderPiece occupant =
    case occupant of
        Player ->
            "@"

        Enemy enemy ->
            "E"

        Brick ->
            "#"

        EmptySpace ->
            "."
