module Board.Types exposing (..)

import Player.Types exposing (..)


type alias Boxes =
    List Box


type alias Coordinate =
    Int


{-| Can not be a union type because I need it as a key in a Dict
-}
type alias Point =
    ( Coordinate, Coordinate )


{-| Can not be a union type because I need it as a key in a Dict
-}
type alias Line =
    ( Point, Point )


type alias Box =
    { up : Line
    , down : Line
    , left : Line
    , right : Line
    , doneBy : Maybe PlayerStatus
    }


type alias BoardSize =
    { width : Int
    , height : Int
    }


updateWidth : Int -> BoardSize -> BoardSize
updateWidth newWidth { width, height } =
    BoardSize newWidth height


updateHeight : Int -> BoardSize -> BoardSize
updateHeight newHeight { width, height } =
    BoardSize width newHeight
