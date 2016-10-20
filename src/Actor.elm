module Actor exposing (..)

import Point exposing (..)


type alias Actor =
    { coords : Point
    , health : Int
    }


new : Point -> Actor
new location =
    { coords = location
    , health = 100
    }
