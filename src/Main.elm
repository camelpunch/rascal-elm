module Main exposing (..)

import Application
import Html
import Model exposing (..)
import Random
import SharedStyles exposing (columnWidth)
import Update exposing (update, subscriptions)
import View exposing (view)


main : Program Never Model Application.Msg
main =
    Html.program
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
    { board = ( columnWidth, 20 )
    , player =
        { coords = { x = 5, y = 5 }
        , health = 100
        }
    , monsters =
        [ { coords = { x = 15, y = 10 }
          , health = 100
          }
        , { coords = { x = 19, y = 10 }
          , health = 100
          }
        ]
    }
