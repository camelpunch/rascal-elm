module Model exposing (..)


type alias Model =
    { board : Board
    , player : Point
    }


type alias Board =
    ( Int, Int )


type alias Point =
    { x : Int
    , y : Int
    }


type Occupant
    = Player
    | Brick
    | EmptySpace


type alias Neighbours =
    { left : Occupant
    , right : Occupant
    , up : Occupant
    , down : Occupant
    }


type Action
    = Occupy
    | DoNothing


type Key
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Unknown
