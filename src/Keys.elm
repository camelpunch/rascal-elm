module Keys exposing (Key, requestFromKeyCode)

import Request exposing (..)


type Key
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | H
    | J
    | K
    | L
    | Unknown


requestFromKeyCode : Int -> Maybe Request
requestFromKeyCode keyCode =
    requestFromKey (keyFromCode keyCode)


requestFromKey : Key -> Maybe Request
requestFromKey key =
    case key of
        ArrowLeft ->
            Just MoveLeft

        H ->
            Just MoveLeft

        ArrowRight ->
            Just MoveRight

        L ->
            Just MoveRight

        ArrowUp ->
            Just MoveUp

        K ->
            Just MoveUp

        ArrowDown ->
            Just MoveDown

        J ->
            Just MoveDown

        Unknown ->
            Nothing


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

        72 ->
            H

        74 ->
            J

        75 ->
            K

        76 ->
            L

        _ ->
            Unknown
