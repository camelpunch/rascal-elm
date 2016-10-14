module Main exposing (..)

import Html.App
import Model exposing (Model, Point, Key(..))
import Random
import Update exposing (update, subscriptions)
import View exposing (view)


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Update.Msg )
init =
    ( model
    , Random.generate Update.NewGame
        (let
            ( width, height ) =
                model.board
         in
            (Random.pair
                (Random.int 1 (width - 1))
                (Random.int 1 (height - 1))
            )
        )
    )


model : Model
model =
    { board = ( 30, 20 )
    , player = { x = 5, y = 5 }
    }
