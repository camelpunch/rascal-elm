module Tests exposing (..)

import Expect
import Fuzz exposing (..)
import Model exposing (Model)
import Occupant exposing (..)
import Request exposing (..)
import Test exposing (..)
import Update exposing (Msg(..))


all : Test
all =
    describe "Game"
        [ test "board has empty space" <|
            \_ ->
                Expect.equal
                    EmptySpace
                    (Update.cellOccupant { x = 1, y = 1 } emptyBoardWithPlayer)
        , test "player is at expected position" <|
            \_ ->
                Expect.equal
                    Player
                    (Update.cellOccupant emptyBoardWithPlayer.player.coords emptyBoardWithPlayer)
        , describe "movement"
            [ fuzzWith { runs = 10000 }
                (Fuzz.list movement)
                "never results in being inside wall"
                (\requests ->
                    (playerInsideWall requests emptyBoardWithPlayer)
                        |> Expect.false "player inside wall"
                )
            ]
          -- , describe "combat"
          --     [ test "player triggers an attack when moving into a monster" <|
          --         \_ ->
          --             Expect.equal ( playerToRightOfMonster, AttackRoll )
          --                 (Update.processRequest (Just MoveLeft) playerToRightOfMonster)
          --     ]
        ]


emptyBoardWithPlayer : Model
emptyBoardWithPlayer =
    { board = ( 5, 5 )
    , player =
        { coords = { x = 2, y = 2 }
        , attacking = Nothing
        }
    , monsters = []
    }


playerToRightOfMonster : Model
playerToRightOfMonster =
    { emptyBoardWithPlayer
        | monsters =
            [ { coords = { x = 1, y = 2 }
              , attacking = Nothing
              }
            ]
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
