module Model exposing (..)


type alias Model =
    { board : Board
    , player : Point
    }


type alias Point =
    { x : Int
    , y : Int
    }


type alias Board =
    ( Int, Int )


type Key
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Unknown
