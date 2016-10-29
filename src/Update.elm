module Update exposing (subscriptions, update)

import Action exposing (..)
import Application exposing (update, Msg(..))
import Keyboard
import Model exposing (..)
import Random


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, newMsg ) =
            Application.update msg model
    in
        ( newModel, cmd newMsg )


cmd : Maybe Msg -> Cmd Msg
cmd m =
    case m of
        Just (Roll action) ->
            Random.generate
                (DieFace action)
                (Random.int 1 10)

        _ ->
            Cmd.none
