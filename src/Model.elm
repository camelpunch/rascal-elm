module Model exposing (..)

import Actor exposing (..)


type alias Model =
    { board : Board
    , player : Actor
    , monsters : List Actor
    }


type alias Board =
    ( Int, Int )
