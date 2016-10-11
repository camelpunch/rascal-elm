module Update exposing (..)

import Keyboard
import Model exposing (Model, Point, Key(..), Piece(..))


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
            move -1 0 model

        ArrowRight ->
            move 1 0 model

        ArrowUp ->
            move 0 -1 model

        ArrowDown ->
            move 0 1 model

        Unknown ->
            model


move : Int -> Int -> Model -> Model
move x y model =
    let
        candidate =
            movePoint x y model.player
    in
        { model | player = candidate }


movePoint : Int -> Int -> Point -> Point
movePoint x y point =
    { point | x = point.x + x, y = point.y + y }


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
