module Update exposing (Msg(..), subscriptions, update, cellOccupant, processRequest)

import Action exposing (..)
import Actor exposing (..)
import Keyboard
import Keys
import Model exposing (Model, Board)
import Neighbours exposing (..)
import Occupant exposing (..)
import Point exposing (..)
import Random
import Request exposing (..)


type Msg
    = NewGame ( Int, Int )
    | KeyDown Keyboard.KeyCode
    | AttackPower Int


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown keyCode ->
            processRequest (Keys.requestFromKeyCode keyCode) model

        NewGame ( playerX, playerY ) ->
            ( { model | player = (move { x = playerX, y = playerY } model.player) }
            , Cmd.none
            )

        AttackPower power ->
            ( model
            , Cmd.none
            )


processRequest : Maybe Request -> Model -> ( Model, Cmd Msg )
processRequest request model =
    let
        ( action, newPosition ) =
            case request of
                Just request ->
                    requestedAction
                        request
                        model.player.coords
                        (neighbours model.player.coords model)

                Nothing ->
                    ( Occupy, model.player.coords )
    in
        case action of
            Occupy ->
                ( { model | player = (move newPosition model.player) }
                , Cmd.none
                )

            Attack ->
                ( { model | player = (attacking newPosition model.player) }
                , Random.generate AttackPower (Random.int 1 6)
                )


attacking : Point -> Actor -> Actor
attacking victim player =
    { player | attacking = Just victim }


move : Point -> Actor -> Actor
move newPosition player =
    { player | coords = newPosition }


requestedAction : Request -> Point -> Neighbours -> ( Action, Point )
requestedAction request point neighbours =
    let
        ( occupant, destination ) =
            case request of
                MoveLeft ->
                    neighbours.left

                MoveRight ->
                    neighbours.right

                MoveUp ->
                    neighbours.up

                MoveDown ->
                    neighbours.down
    in
        case occupant of
            EmptySpace ->
                ( Occupy, destination )

            _ ->
                ( Occupy, point )


neighbours : Point -> Model -> Neighbours
neighbours actor model =
    { left = ( cellOccupant (leftOf actor) model, leftOf actor )
    , right = ( cellOccupant (rightOf actor) model, rightOf actor )
    , up = ( cellOccupant (above actor) model, above actor )
    , down = ( cellOccupant (below actor) model, below actor )
    }


cellOccupant : Point -> Model -> Occupant
cellOccupant point { board, player } =
    let
        ( width, height ) =
            board
    in
        if point.x == 0 || point.x == width || point.y == 0 || point.y == height then
            Brick
        else if point == player.coords then
            PlayerTile
        else
            EmptySpace


above : Point -> Point
above point =
    { point | y = point.y - 1 }


below : Point -> Point
below point =
    { point | y = point.y + 1 }


leftOf : Point -> Point
leftOf point =
    { point | x = point.x - 1 }


rightOf : Point -> Point
rightOf point =
    { point | x = point.x + 1 }
