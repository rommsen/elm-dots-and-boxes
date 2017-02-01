module App.Types exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Form.Validation exposing (..)


type alias Model =
    { game : Maybe Game
    , gameForm : GameForm
    , currentPlayer : Maybe Player
    , playerForm : PlayerForm
    , openGames : List Game
    }



-- , currentGameId : Maybe
-- , playerInCurrentGame : Maybe PlayerStatus
-- game sollte wohl Maybe sein


type alias PlayerId =
    String


type alias Player =
    { id : PlayerId
    , name : String
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
    , doneBy : Maybe PlayerStatus
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


type alias PlayerForm =
    { name : String
    , errors : List Error
    }


defaultPlayerForm : PlayerForm
defaultPlayerForm =
    PlayerForm "" []


type alias GameForm =
    { width : String
    , height : String
    , errors : List Error
    }


defaultGameForm : GameForm
defaultGameForm =
    GameForm "3" "3" []


type alias SelectedLines =
    Dict Line PlayerStatus


type alias Game =
    { id : String
    , playerNames : List String
    , boardSize : BoardSize
    , boxes : Boxes
    , selectedLines : SelectedLines
    , status : GameStatus
    , currentPlayer : PlayerStatus
    , playerPoints : PlayerPoints
    }



-- , players : Dict String PlayerInGame


type alias GameId =
    String


type GameStatus
    = Open
    | Running
    | Winner PlayerStatus
    | Draw


type PlayerStatus
    = Player1
    | Player2
    | Pending


type PlayerInGame
    = PlayerInGame Player PlayerStatus


type alias PlayerPoints =
    ( Int, Int )


type Msg
    = RegisterCurrentPlayer
    | InputPlayerName String
    | CurrentPlayerRegistered Player
    | OpenGame
    | StartGame
    | JoinGame GameId
    | GameOpened String
    | GameChanged JD.Value
    | Select Line
    | InputWidth String
    | InputHeight String
    | OpenGameAdded JD.Value
