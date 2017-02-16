module Board.Types exposing (..)

import Player.Types exposing (PlayerStatus)


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


buildBoxes : BoardSize -> Boxes
buildBoxes { width, height } =
    rows (List.range 0 (height - 1)) (List.range 0 (width - 1))


rows : List Coordinate -> List Coordinate -> List Box
rows ys xs =
    List.foldr (\y boxes -> row y xs ++ boxes) [] ys


row : Coordinate -> List Coordinate -> List Box
row y xs =
    List.foldr (\x boxes -> (buildBox x y) :: boxes) [] xs


buildBox : Coordinate -> Coordinate -> Box
buildBox x y =
    Box
        ( ( x, y )
        , ( x + 1, y )
        )
        ( ( x, y + 1 )
        , ( x + 1, y + 1 )
        )
        ( ( x, y )
        , ( x, y + 1 )
        )
        ( ( x + 1, y )
        , ( x + 1, y + 1 )
        )
        Nothing
