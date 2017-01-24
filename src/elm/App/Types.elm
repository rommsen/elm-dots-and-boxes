module App.Types exposing (..)

import Dict exposing (Dict)


type alias Model =
    { boxes : Boxes
    , selectedLines : SelectedLines
    , boardSize : BoardSize
    , game : Game
    , currentPlayer : Player
    , playerPoints : PlayerPoints
    }


type alias Boxes =
    List Box


{-| Can not be a union type because I need it as a key in a Dict
-}
type alias Point =
    ( Int, Int )


{-| Can not be a union type because I need it as a key in a Dict
-}
type alias Line =
    ( Point, Point )


type alias Box =
    { up : Line
    , down : Line
    , left : Line
    , right : Line
    , doneBy : Maybe Player
    }


type BoardSize
    = BoardSize Int Int


type alias SelectedLines =
    Dict Line Player


type Game
    = NotStarted
    | Winner Player
    | Process


type Player
    = Player1
    | Player2


type alias PlayerPoints =
    ( Int, Int )


type Msg
    = Start
    | Select Line
