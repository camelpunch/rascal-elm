module Application exposing (cellOccupant, processRequest, update, Msg(..))

import Action exposing (..)
import Actor exposing (..)
import Keyboard
import Keys
import Model exposing (Model, Board)
import Neighbours exposing (..)
import Occupant exposing (..)
import Point exposing (..)
import Request exposing (..)


type Msg
    = NewGame ( Int, Int )
    | KeyDown Keyboard.KeyCode
    | Roll Action
    | DieFace Action Int


update : Msg -> Model -> ( Model, Maybe Msg )
update msg model =
    case msg of
        KeyDown keyCode ->
            processRequest (Keys.requestFromKeyCode keyCode) model

        NewGame ( playerX, playerY ) ->
            ( { model | player = (move { x = playerX, y = playerY } model.player) }
            , Nothing
            )

        DieFace (Attack perp victim) attackStrength ->
            ( damage victim attackStrength model
            , if isCounterAttack victim model then
                Nothing
              else
                Just (Roll (Attack victim model.player))
            )

        _ ->
            ( model
            , Nothing
            )


isCounterAttack : Actor -> Model -> Bool
isCounterAttack victim model =
    victim == model.player


damage : Actor -> Int -> Model -> Model
damage victim attackStrength model =
    { model
        | monsters =
            List.map
                (\m ->
                    if victim == m then
                        applyDamage attackStrength victim
                    else
                        m
                )
                model.monsters
        , player =
            if victim == model.player then
                applyDamage attackStrength victim
            else
                model.player
    }


applyDamage attackStrength victim =
    { victim | health = victim.health - (attackStrength * 10) }


processRequest : Maybe Request -> Model -> ( Model, Maybe Msg )
processRequest request model =
    case request of
        Just request ->
            case
                requestedAction
                    request
                    model.player
                    (neighbours model.player.coords model)
            of
                Occupy position ->
                    ( { model | player = move position model.player }
                    , Nothing
                    )

                Attack perp victim ->
                    ( model
                    , Just (Roll (Attack perp victim))
                    )

        Nothing ->
            ( model, Nothing )


requestedAction : Request -> Actor -> Neighbours -> Action
requestedAction request player neighbours =
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
                Occupy destination

            Brick ->
                Occupy player.coords

            Player ->
                Occupy player.coords

            Enemy enemy ->
                Attack player enemy


move : Point -> Actor -> Actor
move newPosition player =
    { player | coords = newPosition }


neighbours : Point -> Model -> Neighbours
neighbours actor model =
    { left = ( cellOccupant (leftOf actor) model, leftOf actor )
    , right = ( cellOccupant (rightOf actor) model, rightOf actor )
    , up = ( cellOccupant (above actor) model, above actor )
    , down = ( cellOccupant (below actor) model, below actor )
    }


cellOccupant : Point -> Model -> Occupant
cellOccupant point { board, player, monsters } =
    let
        ( width, height ) =
            board

        monsterAtPoint =
            List.head
                (List.filter (\m -> m.coords == point)
                    monsters
                )
    in
        case monsterAtPoint of
            Just monster ->
                Enemy monster

            Nothing ->
                if point.x == 0 || point.x == width || point.y == 0 || point.y == height then
                    Brick
                else if point == player.coords then
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
