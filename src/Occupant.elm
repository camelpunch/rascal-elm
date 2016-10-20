module Occupant exposing (..)

import Actor exposing (..)


type Occupant
    = Player
    | Enemy Actor
    | Brick
    | EmptySpace
