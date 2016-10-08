module Main exposing (..)

import Html exposing (Html, text)
import Html.App
import Keyboard


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { board : List Int
    , player : Point
    }


type alias Point =
    { x : Int
    , y : Int
    }


type Key
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Unknown


model : Model
model =
    { board = [ 20, 20 ]
    , player = { x = 5, y = 5 }
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


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
    { model | player = movePoint x y model.player }


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



-- VIEW


view : Model -> Html msg
view model =
    text (toString model)



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
