module Tests exposing (..)

import Expect
import Fuzz exposing (..)
import Model exposing (Model, Request(..))
import String
import Test exposing (..)
import Update


all : Test
all =
    let
        ( width, height ) =
            model.board
    in
        describe "Game"
            [ describe "Board"
                [ test "has empty space" <|
                    \_ ->
                        Expect.equal
                            Model.EmptySpace
                            (Update.cellOccupant { x = 1, y = 1 } model)
                , test "Player is at expected position" <|
                    \_ ->
                        Expect.equal
                            Model.Player
                            (Update.cellOccupant model.player model)
                ]
            , describe "Movement"
                [ fuzzWith { runs = 500 }
                    (Fuzz.list movement)
                    "Never results in being inside right-hand wall"
                    (getPlayerX model >> Expect.notEqual width)
                , fuzzWith { runs = 500 }
                    (Fuzz.list movement)
                    "Never results in being inside left-hand wall"
                    (getPlayerX model >> Expect.greaterThan 0)
                ]
            ]


model : Model
model =
    { board = ( 5, 5 )
    , player = { x = 2, y = 2 }
    }


getPlayerX : Model -> List Request -> Int
getPlayerX beforeState requests =
    let
        afterState =
            modelAfterMovements requests beforeState
    in
        afterState.player.x


modelAfterMovements : List Request -> Model -> Model
modelAfterMovements requests beforeState =
    List.foldl dispatchRequest beforeState requests


dispatchRequest : Request -> Model -> Model
dispatchRequest request state =
    Update.processRequest (Just request) state


movement : Fuzzer Model.Request
movement =
    Fuzz.map request (Fuzz.intRange 0 3)


request : Int -> Model.Request
request n =
    if n == 0 then
        MoveLeft
    else if n == 1 then
        MoveRight
    else if n == 2 then
        MoveUp
    else
        MoveDown
