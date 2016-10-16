module Update exposing (subscriptions, update)

import Application exposing (update, Msg(..))
import Keyboard
import Model exposing (Model)
import Random


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, newMsg ) =
            Application.update msg model

        newCmd =
            case newMsg of
                Nothing ->
                    Cmd.none

                Just m ->
                    Cmd.none
    in
        ( newModel, newCmd )
