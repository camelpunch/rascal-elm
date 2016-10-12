module Update exposing (Msg(..), subscriptions, update, modelAfterMovement, keyFromCode)

import Keyboard
import Model exposing (Model, Point, Key(..), Action(..), Occupant(..), Neighbours)


type Msg
    = KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( model, Cmd.none )


keyDown : Keyboard.KeyCode -> Model -> Model
keyDown keyCode model =
    modelAfterMovement (keyFromCode keyCode) model


modelAfterMovement : Model.Key -> Model -> Model
modelAfterMovement key model =
    case actionFromKey key model.player (neighbours model.player model) of
        ( Occupy, newPosition ) ->
            { model | player = newPosition }

        ( DoNothing, newPosition ) ->
            model


neighbours : Point -> Model -> Model.Neighbours
neighbours actor model =
    { left = cellOccupant { actor | x = actor.x - 1 } model
    , right = cellOccupant { actor | x = actor.x + 1 } model
    , up = cellOccupant { actor | y = actor.y - 1 } model
    , down = cellOccupant { actor | y = actor.y + 1 } model
    }


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


actionFromKey : Key -> Point -> Neighbours -> ( Action, Point )
actionFromKey key point neighbours =
    case ( key, neighbours.left, neighbours.right, neighbours.up, neighbours.down ) of
        ( ArrowLeft, EmptySpace, _, _, _ ) ->
            ( Occupy, leftOf point )

        ( ArrowRight, _, EmptySpace, _, _ ) ->
            ( Occupy, rightOf point )

        ( ArrowUp, _, _, EmptySpace, _ ) ->
            ( Occupy, above point )

        ( ArrowDown, _, _, _, EmptySpace ) ->
            ( Occupy, below point )

        ( ArrowLeft, Player, _, _, _ ) ->
            ( DoNothing, leftOf point )

        ( ArrowRight, _, Player, _, _ ) ->
            ( DoNothing, rightOf point )

        ( ArrowUp, _, _, Player, _ ) ->
            ( DoNothing, above point )

        ( ArrowDown, _, _, _, Player ) ->
            ( DoNothing, below point )

        ( ArrowLeft, Brick, _, _, _ ) ->
            ( DoNothing, point )

        ( ArrowRight, _, Brick, _, _ ) ->
            ( DoNothing, point )

        ( ArrowUp, _, _, Brick, _ ) ->
            ( DoNothing, point )

        ( ArrowDown, _, _, _, Brick ) ->
            ( DoNothing, point )

        ( Unknown, _, _, _, _ ) ->
            ( DoNothing, point )


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


cellOccupant : Point -> Model -> Occupant
cellOccupant { x, y } model =
    let
        ( width, height ) =
            model.board
    in
        if x == 0 || x == width || y == 0 || y == height then
            Brick
        else if model.player.x == x && model.player.y == y then
            Player
        else
            EmptySpace
