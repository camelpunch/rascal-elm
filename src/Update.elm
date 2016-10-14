module Update exposing (Msg(..), subscriptions, update, cellOccupant, processRequest)

import Keyboard
import Keys
import Model exposing (Model, Board, Point, Request(..), Key(..), Action(..), Occupant(..), Neighbours)


type Msg
    = KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | NewGame ( Int, Int )


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
            processRequest (Keys.requestFromKeyCode keyCode) model

        KeyUp keyCode ->
            ( model, Cmd.none )

        NewGame ( x, y ) ->
            ( { model | player = { x = x, y = y } }, Cmd.none )


processRequest : Maybe Request -> Model -> ( Model, Cmd Msg )
processRequest request model =
    let
        ( action, newPosition ) =
            case request of
                Just request ->
                    requestedAction
                        request
                        model.player
                        (neighbours model.player model)

                Nothing ->
                    ( Occupy, model.player )
    in
        case action of
            Occupy ->
                ( { model | player = newPosition }, Cmd.none )


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
        else if point == player then
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
