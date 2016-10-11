module Model exposing (..)


type alias Model =
    { board : Board
    , player : Point
    }


type alias Point =
    { x : Int
    , y : Int
    }


type alias Vector =
    Point


type Piece
    = Player
    | Brick
    | EmptySpace


type alias Board =
    ( Int, Int )


type Key
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Unknown


cell : Point -> Model -> Piece
cell { x, y } model =
    let
        ( width, height ) =
            model.board
    in
        if x == 0 || x == width || y == 0 || y == height then
            Brick
        else if model.player.x == x && model.player.y == y then
            Player
        else
            EmptySpace
