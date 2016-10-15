module Player exposing (..)

import Point exposing (..)


type alias Player =
    { coords : Point
    , attacking : Maybe Point
    }
