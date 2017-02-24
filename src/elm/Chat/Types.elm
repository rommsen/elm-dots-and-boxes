module Chat.Types exposing (..)

import Game.Types exposing (GameId)
import Player.Types exposing (Player)


type alias Message =
    { msg : String
    , player : Player
    , gameId : GameId
    }
