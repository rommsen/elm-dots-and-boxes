module Types exposing (..)

import Date
import Dict exposing (Dict)
import Json.Decode as JD
import Board.Types exposing (..)
import Game.Types exposing (..)
import Player.Types exposing (..)


type alias Model =
    { game : Maybe Game
    , boardSize : BoardSize
    , localPlayer : Maybe Player
    , playerForm : PlayerForm
    , openGames : Dict GameId Game
    , runningGames : Dict GameId Game
    }



{-

    Ich muss unterscheiden zwischen: Spiel wurde erstellt und gestartet.
    Bei einem erstellten Spiel können sich noch andere Leute verbinden.


    Man eröffnet ein Spiel, damit hört man nur noch auf Änderungen an diesem Spiel

    Wenn man teilnimmt ebenso

    Wie handele ich Join Requests?
    Ich kann nicht einfach das Game ändern, da dies zu Race Conditions führen kann

    vllt könnte ich einfach eine neue Liste mit Join Requests bauen?
    und der Owner kann dann mit dieser arbeiten

    könnte man vllt nicht das ganze Game ändern (also auch serialisieren?), sondern
    nur an join requests pushen? ich sollte ja trotzdem das change event überall bekommen?!?

    Wenn ich ein game join, muss dieses game direkt das game werden

    sollte man die JoinRequest auch als Dict machen?

    Wenn dies geht könnte man doch wieder "eine" Player Liste machen
    in dieser Liste steht dann mein status

    Wie komme ich an den nächsten Spieler?

    irgendwie muss ich wissen, welches der nächste Spieler nach einem Zug ist
    Idealerweise bekomme ich eine PlayerId, welche ich dann nehmen kann, um auf
    das Players Dict zugreifen zu können

    Players =
     { previous : List PlayerInGame
     , current: PlayerInGame
     , next: List PlayerInGame
   }

   wenn next leer ist fange wieder vorne an

-}


type Msg
    = RegisterLocalPlayer
    | InputPlayerName String
    | LocalPlayerRegistered Player
    | CreateGame
    | OpenGame Player Date.Date
    | StartGame
    | RequestToJoinGame Game
    | WatchGame Game
    | AcceptPlayer JoinGameRequestEntry
    | GameOpened String
    | GameChanged JD.Value
    | Select Line
    | InputWidth Int
    | InputHeight Int
    | OpenGameAdded JD.Value
    | OpenGameRemoved GameId
    | RunningGameAdded JD.Value
    | RunningGameRemoved GameId
    | BackToLobby
