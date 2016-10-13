module Update exposing (Msg(..), subscriptions, update, processKey, keyFromCode, cellOccupant)

import Keyboard
import Model exposing (Model, Point, Request(..), Key(..), Action(..), Occupant(..), Neighbours)


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
    processKey (keyFromCode keyCode) model


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


processKey : Model.Key -> Model -> Model
processKey key model =
    let
        getAction =
            \direction ->
                actionFromRequest
                    direction
                    model.player
                    (neighbours model.player model)

        ( action, newPosition ) =
            case key of
                ArrowLeft ->
                    getAction MoveLeft

                ArrowRight ->
                    getAction MoveRight

                ArrowUp ->
                    getAction MoveUp

                ArrowDown ->
                    getAction MoveDown

                Unknown ->
                    ( DoNothing, model.player )
    in
        case action of
            Occupy ->
                { model | player = newPosition }

            DoNothing ->
                model


actionFromRequest : Request -> Point -> Neighbours -> ( Action, Point )
actionFromRequest request point neighbours =
    case ( request, neighbours.left, neighbours.right, neighbours.up, neighbours.down ) of
        ( MoveLeft, EmptySpace, _, _, _ ) ->
            ( Occupy, leftOf point )

        ( MoveRight, _, EmptySpace, _, _ ) ->
            ( Occupy, rightOf point )

        ( MoveUp, _, _, EmptySpace, _ ) ->
            ( Occupy, above point )

        ( MoveDown, _, _, _, EmptySpace ) ->
            ( Occupy, below point )

        ( MoveLeft, Player, _, _, _ ) ->
            ( DoNothing, leftOf point )

        ( MoveRight, _, Player, _, _ ) ->
            ( DoNothing, rightOf point )

        ( MoveUp, _, _, Player, _ ) ->
            ( DoNothing, above point )

        ( MoveDown, _, _, _, Player ) ->
            ( DoNothing, below point )

        ( MoveLeft, Brick, _, _, _ ) ->
            ( DoNothing, point )

        ( MoveRight, _, Brick, _, _ ) ->
            ( DoNothing, point )

        ( MoveUp, _, _, Brick, _ ) ->
            ( DoNothing, point )

        ( MoveDown, _, _, _, Brick ) ->
            ( DoNothing, point )


neighbours : Point -> Model -> Neighbours
neighbours actor model =
    { left = cellOccupant (leftOf actor) model
    , right = cellOccupant (rightOf actor) model
    , up = cellOccupant (above actor) model
    , down = cellOccupant (below actor) model
    }


cellOccupant : Point -> Model -> Occupant
cellOccupant { x, y } { board, player } =
    let
        ( width, height ) =
            board
    in
        if x == 0 || x == width || y == 0 || y == height then
            Brick
        else if x == player.x && y == player.y then
            Player
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
