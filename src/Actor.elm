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


dead : Actor -> Actor
dead actor =
    { actor | health = 0 }


isDead : Actor -> Bool
isDead actor =
    actor.health <= 0
