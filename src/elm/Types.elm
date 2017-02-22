module Types exposing (..)

import Board.Types exposing (..)
import Date
import Dict exposing (Dict)
import Game.Types exposing (..)
import Json.Decode as JD
import Player.Types exposing (..)
import Time


type alias Model =
    { game : Maybe Game
    , gameForm : GameForm
    , localPlayer : Maybe Player
    , playerForm : PlayerForm
    , openGames : Dict GameId Game
    , runningGames : Dict GameId Game
    , turnTimer : TurnTimer
    }


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
    | InputTurnTimer TurnTimer
    | TurnTimerTick Time.Time
