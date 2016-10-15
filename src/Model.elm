module Model exposing (..)

import Player exposing (..)


type alias Model =
    { board : Board
    , player : Player
    }


type alias Board =
    ( Int, Int )
