module Neighbours exposing (..)

import Point exposing (..)
import Occupant exposing (..)


type alias Neighbours =
    { left : ( Occupant, Point )
    , right : ( Occupant, Point )
    , up : ( Occupant, Point )
    , down : ( Occupant, Point )
    }
