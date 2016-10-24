module Main exposing (..)

import Application
import Html.App
import Model exposing (..)
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


init : ( Model, Cmd Application.Msg )
init =
    ( model
    , Random.generate Application.NewGame
        (let
            ( width, height ) =
                model.board

            boundingWallDepth =
                1
         in
            (Random.pair
                (Random.int boundingWallDepth (width - boundingWallDepth))
                (Random.int boundingWallDepth (height - boundingWallDepth))
            )
        )
    )


model : Model
model =
    { board = ( 30, 20 )
    , player =
        { coords = { x = 5, y = 5 }
        , health = 100
        }
    , monsters =
        [ { coords = { x = 15, y = 10 }
          , health = 100
          }
        ]
    }
