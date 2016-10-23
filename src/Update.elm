module Update exposing (subscriptions, update)

import Action exposing (..)
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
                    case m of
                        Roll (Attack perp victim) ->
                            Random.generate
                                (DieFace (Attack perp victim))
                                (Random.int 1 6)

                        _ ->
                            Cmd.none
    in
        ( newModel, newCmd )
