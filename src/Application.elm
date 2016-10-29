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
            , Just (Roll (CounterAttack victim model.player))
            )

        DieFace (CounterAttack perp victim) attackStrength ->
            ( damage victim attackStrength model
            , Nothing
            )

        _ ->
            ( model
            , Nothing
            )


damage : Actor -> Int -> Model -> Model
damage victim attackStrength model =
    { model
        | monsters = damageInGroup attackStrength victim model.monsters
        , player =
            case damageInGroup attackStrength victim [ model.player ] of
                [] ->
                    model.player

                actor :: _ ->
                    actor
    }


damageInGroup : Int -> Actor -> (List Actor -> List Actor)
damageInGroup attackStrength victim =
    List.foldl
        (\actor survivors ->
            let
                damaged =
                    applyDamage attackStrength

                survivor =
                    if victim == actor && isDead (damaged victim) then
                        []
                    else if victim == actor then
                        [ damaged victim ]
                    else
                        [ actor ]
            in
                List.append survivors survivor
        )
        []


applyDamage : Int -> Actor -> Actor
applyDamage attackStrength victim =
    { victim | health = victim.health - (attackStrength * 10) }


isDead : Actor -> Bool
isDead actor =
    actor.health <= 0


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

                _ ->
                    ( model, Nothing )

        Nothing ->
            ( model, Nothing )


requestedAction : Request -> Actor -> Neighbours -> Action
requestedAction request actor neighbours =
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
                Occupy actor.coords

            Player ->
                Occupy actor.coords

            Enemy enemy ->
                Attack actor enemy


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
