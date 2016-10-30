module Tests exposing (..)

import Action exposing (..)
import Actor exposing (..)
import Application exposing (cellOccupant, Msg(..))
import Expect
import Fuzz exposing (..)
import Model exposing (Model)
import Occupant exposing (..)
import Request exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Game"
        [ test "board has empty space" <|
            \_ ->
                Expect.equal
                    EmptySpace
                    (Application.cellOccupant { x = 1, y = 1 } emptyBoardWithPlayer)
        , test "player is at expected position" <|
            \_ ->
                Expect.equal
                    Player
                    (Application.cellOccupant emptyBoardWithPlayer.player.coords emptyBoardWithPlayer)
        , describe "movement"
            [ fuzzWith { runs = 1000 }
                (Fuzz.list movement)
                "never results in being inside wall"
                (\requests ->
                    (playerInsideWall requests emptyBoardWithPlayer)
                        |> Expect.false "player inside wall"
                )
            ]
        , describe "combat"
            [ test "player triggers an attack when moving into a monster" <|
                \_ ->
                    Expect.equal
                        ( playerToRightOfMonster, Just (Roll (Attack playerToRightOfMonster.player secondMonster)) )
                        (Application.processRequest (Just MoveLeft) playerToRightOfMonster)
            , test "attack causes damage to monster" <|
                \_ ->
                    Expect.equal
                        [ 100, 40 ]
                        (let
                            ( newState, _ ) =
                                (Application.update
                                    (DieFace (Attack playerToRightOfMonster.player secondMonster) 6)
                                    playerToRightOfMonster
                                )
                         in
                            List.map .health newState.monsters
                        )
            , test "monster ceases to exist when health reaches zero" <|
                \_ ->
                    Expect.equal
                        [ firstMonster ]
                        (let
                            ( newState, _ ) =
                                (Application.update
                                    (DieFace (Attack playerToRightOfMonster.player secondMonster) 10)
                                    playerToRightOfMonster
                                )
                         in
                            newState.monsters
                        )
            , test "attacks trigger counterattacks" <|
                \_ ->
                    Expect.equal
                        (Just (Roll (CounterAttack secondMonster playerToRightOfMonster.player)))
                        (let
                            ( _, cmd ) =
                                (Application.update
                                    (DieFace (Attack playerToRightOfMonster.player secondMonster) 5)
                                    playerToRightOfMonster
                                )
                         in
                            cmd
                        )
            , test "counterattacks damage player" <|
                \_ ->
                    let
                        ( newState, _ ) =
                            (Application.update
                                (DieFace (CounterAttack secondMonster playerToRightOfMonster.player) 6)
                                playerToRightOfMonster
                            )
                    in
                        Expect.equal 40 newState.player.health
            , test "counterattacks can kill player" <|
                \_ ->
                    let
                        ( newState, _ ) =
                            (Application.update
                                (DieFace (CounterAttack secondMonster playerToRightOfMonster.player) 10)
                                playerToRightOfMonster
                            )
                    in
                        Expect.equal 0 newState.player.health
            , test "counterattacks don't trigger more attacks" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (let
                            ( _, cmd ) =
                                (Application.update
                                    (DieFace (CounterAttack secondMonster playerToRightOfMonster.player) 5)
                                    playerToRightOfMonster
                                )
                         in
                            cmd
                        )
            ]
        ]


emptyBoardWithPlayer : Model
emptyBoardWithPlayer =
    { board = ( 5, 5 )
    , player =
        Actor.new { x = 2, y = 2 }
    , monsters = []
    }


playerToRightOfMonster : Model
playerToRightOfMonster =
    { emptyBoardWithPlayer
        | monsters = [ firstMonster, secondMonster ]
        , player =
            Actor.new
                { x = secondMonster.coords.x + 1
                , y = secondMonster.coords.y
                }
    }


firstMonster : Actor
firstMonster =
    Actor.new { x = 1, y = 2 }


secondMonster : Actor
secondMonster =
    Actor.new { x = 3, y = 3 }


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
    fst (Application.processRequest (Just request) state)


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
