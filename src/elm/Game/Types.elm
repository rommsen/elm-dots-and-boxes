module Game.Types exposing (..)

import Date
import Dict exposing (Dict)
import Form.Validation exposing (..)
import Json.Decode as JD
import Player.Types exposing (..)
import Board.Types exposing (..)


type alias Game =
    { id : GameId
    , owner : Player
    , createdAt : Date.Date
    , boardSize : BoardSize
    , boxes : Boxes
    , selectedLines : SelectedLines
    , status : GameStatus
    , result : GameResult
    , players : PlayersInGame
    , availablePlayerStatus : List PlayerStatus
    , joinRequests : Dict JoinGameRequestId Player
    , spectators : Dict JoinGameRequestId Player
    }


type alias GameId =
    String


type alias SelectedLines =
    Dict Line PlayerStatus


type GameStatus
    = Open
    | Running
    | Finished


type GameResult
    = None
    | Winner PlayerInGame
    | Draw (List PlayerInGame)


type alias JoinGameRequestId =
    String


type alias JoinGameRequestEntry =
    ( JoinGameRequestId, Player )


type alias JoinGameRequest =
    { gameId : GameId
    , player : Player
    }
