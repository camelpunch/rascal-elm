module Main exposing (..)

import Html.App
import Model exposing (Model, Point, Key(..))
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
    ( model, Cmd.none )


model : Model
model =
    { board = ( 30, 20 )
    , player = { x = 5, y = 5 }
    }
