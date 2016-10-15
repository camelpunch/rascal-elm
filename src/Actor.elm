module Actor exposing (..)

import Point exposing (..)


type alias Actor =
    { coords : Point
    , attacking : Maybe Point
    }
