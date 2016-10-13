module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (..)
import Model exposing (Model)
import Update
import Expect
import String


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


modelAfterMovements : List Model.Key -> Model -> Model
modelAfterMovements keys beforeState =
    List.foldl moveWithKey beforeState keys


moveWithKey : Model.Key -> Model -> Model
moveWithKey key state =
    Update.processKey key state


getPlayerX : Model -> List Model.Key -> Int
getPlayerX beforeState keys =
    let
        afterState =
            modelAfterMovements keys beforeState
    in
        afterState.player.x


movement : Fuzzer Model.Key
movement =
    Fuzz.map Update.keyFromCode (Fuzz.intRange 37 41)
