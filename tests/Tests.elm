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
        describe "Movement"
            [ fuzzWith { runs = 500 }
                (Fuzz.list movement)
                "Never results in being inside right-hand wall"
                (getPlayerX model >> Expect.notEqual width)
            , fuzzWith { runs = 500 }
                (Fuzz.list movement)
                "Never results in being inside left-hand wall"
                (getPlayerX model >> Expect.greaterThan 0)
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
    Update.modelAfterMovement key state


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
