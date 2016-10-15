module Model exposing (..)

import Player exposing (..)
import Point exposing (..)


type alias Model =
    { board : Board
    , player : Player
    }


type alias Board =
    ( Int, Int )


type Occupant
    = PlayerTile
    | Brick
    | EmptySpace


type alias Neighbours =
    { left : ( Occupant, Point )
    , right : ( Occupant, Point )
    , up : ( Occupant, Point )
    , down : ( Occupant, Point )
    }


type Action
    = Occupy
    | Attack


type Key
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | H
    | J
    | K
    | L
    | Unknown
