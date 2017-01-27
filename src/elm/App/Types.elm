module App.Types exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD


type alias Model =
    { boardSize : BoardSize
    , game : Game
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


type alias Game =
    { id : String
    , playerNames : List String
    , boxes : Boxes
    , selectedLines : SelectedLines
    , status : GameStatus
    , currentPlayer : Player
    , playerPoints : PlayerPoints
    }


type GameStatus
    = NotStarted
    | Winner Player
    | Draw
    | Process


type Player
    = Player1
    | Player2


type alias PlayerPoints =
    ( Int, Int )


type Msg
    = StartGame
    | GameStarted JD.Value
    | GameChanged JD.Value
    | Select Line
