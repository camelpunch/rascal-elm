module View exposing (view)

import Html exposing (Html, text, div, table, tr, td, ul, li)
import Html.CssHelpers
import Model exposing (Model)
import Occupant exposing (..)
import SharedStyles
import Application exposing (cellOccupant)


{ id, class, classList } =
    Html.CssHelpers.withNamespace SharedStyles.rascalNamespace
view : Model -> Html msg
view model =
    let
        ( width, height ) =
            model.board
    in
        div [ id SharedStyles.Page ]
            [ table []
                (List.map (row model) [0..height])
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


row : Model -> Int -> Html msg
row model row =
    let
        ( width, height ) =
            model.board
    in
        tr []
            (List.map
                (\col -> column col row model)
                [0..width]
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
