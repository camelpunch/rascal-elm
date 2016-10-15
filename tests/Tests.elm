module Tests exposing (..)

import Expect
import Fuzz exposing (..)
import Model exposing (Model)
import Request exposing (..)
import Test exposing (..)
import Update


all : Test
all =
    let
        ( width, height ) =
            model.board
    in
        describe "Game"
            [ test "board has empty space" <|
                \_ ->
                    Expect.equal
                        Model.EmptySpace
                        (Update.cellOccupant { x = 1, y = 1 } model)
            , test "player is at expected position" <|
                \_ ->
                    Expect.equal
                        Model.PlayerTile
                        (Update.cellOccupant model.player.coords model)
            , describe "movement"
                [ fuzzWith { runs = 10000 }
                    (Fuzz.list movement)
                    "never results in being inside wall"
                    (\requests ->
                        (playerInsideWall requests model)
                            |> Expect.false "player inside wall"
                    )
                ]
            ]


model : Model
model =
    { board = ( 5, 5 )
    , player =
        { coords = { x = 2, y = 2 }
        , attacking = Nothing
        }
    }


playerInsideWall : List Request -> Model -> Bool
playerInsideWall requests model =
    let
        { x, y } =
            (modelAfterMovements requests model).player.coords

        ( width, height ) =
            model.board
    in
        x == 0 || x == width || y == 0 || y == height


modelAfterMovements : List Request -> Model -> Model
modelAfterMovements requests beforeState =
    List.foldl dispatchRequest beforeState requests


dispatchRequest : Request -> Model -> Model
dispatchRequest request state =
    fst (Update.processRequest (Just request) state)


movement : Fuzzer Request
movement =
    Fuzz.map request (Fuzz.intRange 0 3)


request : Int -> Request
request n =
    case n of
        0 ->
            MoveLeft

        1 ->
            MoveRight

        2 ->
            MoveUp

        _ ->
            MoveDown
