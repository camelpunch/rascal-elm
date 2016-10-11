module Update exposing (..)

import Keyboard
import Model exposing (Model, Point, Vector, Key(..), Piece(..))


type Msg
    = KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( model, Cmd.none )


keyDown : Keyboard.KeyCode -> Model -> Model
keyDown keyCode model =
    case keyFromCode keyCode of
        ArrowLeft ->
            move { x = -1, y = 0 } model

        ArrowRight ->
            move { x = 1, y = 0 } model

        ArrowUp ->
            move { x = 0, y = -1 } model

        ArrowDown ->
            move { x = 0, y = 1 } model

        Unknown ->
            model


move : Vector -> Model -> Model
move vec model =
    let
        candidate =
            movePoint vec model.player
    in
        { model | player = candidate }


movePoint : Vector -> Point -> Point
movePoint vec point =
    { point | x = point.x + vec.x, y = point.y + vec.y }


keyFromCode : Int -> Key
keyFromCode keyCode =
    case keyCode of
        37 ->
            ArrowLeft

        38 ->
            ArrowUp

        39 ->
            ArrowRight

        40 ->
            ArrowDown

        _ ->
            Unknown



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
