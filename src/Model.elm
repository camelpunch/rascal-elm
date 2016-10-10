-- MODEL


module Model exposing (..)


type alias Model =
    { board : Point
    , player : Point
    }


type alias Point =
    { x : Int
    , y : Int
    }


type Key
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Unknown
