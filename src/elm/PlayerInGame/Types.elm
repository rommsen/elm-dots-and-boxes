module PlayerInGame.Types exposing (..)

import Date
import Dict exposing (Dict)
import Form.Validation exposing (..)
import Json.Decode as JD


type alias Players =
    Dict PlayerId PlayerInGame


type PlayersInGame
    = PlayersInGame
        { previous : List PlayerInGame
        , current : PlayerInGame
        , next : List PlayerInGame
        }


createPlayerInGame : Player -> PlayerStatus -> PlayerPoints -> PlayerInGame
createPlayerInGame player status points =
    PlayerInGame { player = player, status = status, points = points }


playersInGameToList : PlayersInGame -> List PlayerInGame
playersInGameToList (PlayersInGame { previous, current, next }) =
    previous
        |> (::) current
        |> (++) next


createPlayersInGame : List PlayerInGame -> PlayerInGame -> List PlayerInGame -> PlayersInGame
createPlayersInGame previous current next =
    PlayersInGame { previous = previous, current = current, next = next }


getCurrentPlayer : PlayersInGame -> PlayerInGame
getCurrentPlayer (PlayersInGame { current }) =
    current


updateCurrentPlayer : PlayersInGame -> PlayerInGame -> PlayersInGame
updateCurrentPlayer (PlayersInGame { previous, next }) newCurrent =
    createPlayersInGame previous newCurrent next


addPlayer : PlayersInGame -> PlayerInGame -> PlayersInGame
addPlayer (PlayersInGame { previous, current, next }) player =
    let
        newNext =
            next
                |> List.reverse
                |> (::) player
                |> List.reverse
    in
        createPlayersInGame previous current newNext


playerIsPlayerInGame : Player -> PlayersInGame -> Bool
playerIsPlayerInGame player players =
    playersInGameToList players
        |> List.map (\(PlayerInGame playerInGame) -> playerInGame.player)
        |> List.member player


numberPlayers : PlayersInGame -> Int
numberPlayers players =
    playersInGameToList players
        |> List.length


playerListSortedByPlayerPoints : PlayersInGame -> List PlayerInGame
playerListSortedByPlayerPoints players =
    playersInGameToList players
        |> List.sortWith comparePlayerPoints
        |> List.reverse


getWinner : PlayersInGame -> List PlayerInGame
getWinner players =
    let
        getPlayersWithHighestPoints : PlayerInGame -> List PlayerInGame -> List PlayerInGame
        getPlayersWithHighestPoints player topList =
            case topList of
                topPlayer :: tail ->
                    case comparePlayerPoints player topPlayer of
                        EQ ->
                            player :: topList

                        LT ->
                            topList

                        GT ->
                            [ player ]

                [] ->
                    [ player ]
    in
        players
            |> playersInGameToList
            |> List.foldl getPlayersWithHighestPoints []


comparePlayerPoints : PlayerInGame -> PlayerInGame -> Order
comparePlayerPoints (PlayerInGame playerA) (PlayerInGame playerB) =
    compare playerA.points playerB.points
