module Update exposing (subscriptions, update)

import Action exposing (..)
import Application exposing (update, Msg(..))
import Array
import Keyboard
import Model exposing (..)
import Random exposing (generate, int, list)


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
        Just (Roll []) ->
            Cmd.none

        Just (Roll actions) ->
            let
                actionsCount =
                    List.length actions
            in
                generate
                    Dice
                    (Random.map
                        (List.map2 (,) actions)
                        (list actionsCount (int 1 10))
                    )

        _ ->
            Cmd.none
