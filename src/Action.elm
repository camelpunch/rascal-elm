module Action exposing (..)

import Actor exposing (..)
import Point exposing (..)


type Action
    = Occupy Point
    | Attack Actor
