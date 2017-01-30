module App.Types exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Form.Validation exposing (..)


type alias Model =
    { game : Maybe Game
    , playerName : String
    , gameForm : GameForm
    }



-- , currentGameId : Maybe
-- , playerInCurrentGame : Maybe Player
-- game sollte wohl Maybe sein


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


type alias BoardSize =
    { width : Int
    , height : Int
    }



{-

   Ich muss unterscheiden zwischen: Spiel wurde erstellt und gestartet.
   Bei einem erstellten Spiel können sich noch andere Leute verbinden.


   Man eröffnet ein Spiel, damit hört man nur noch auf Änderungen an diesem Spiel

   Wenn man teilnimmt ebenso

-}


type alias GameForm =
    { width : String
    , height : String
    , errors : List Error
    }


defaultGameForm : GameForm
defaultGameForm =
    GameForm "3" "3" []


type alias SelectedLines =
    Dict Line Player


type alias Game =
    { id : String
    , playerNames : List String
    , boardSize : BoardSize
    , boxes : Boxes
    , selectedLines : SelectedLines
    , status : GameStatus
    , currentPlayer : Player
    , playerPoints : PlayerPoints
    }


type alias GameId =
    String


type GameStatus
    = Open
    | Running
    | Winner Player
    | Draw


type Player
    = Player1
    | Player2


type alias PlayerPoints =
    ( Int, Int )


type Msg
    = OpenGame
    | StartGame
    | JoinGame GameId
    | GameOpened String
    | GameChanged JD.Value
    | Select Line
    | InputWidth String
    | InputHeight String
