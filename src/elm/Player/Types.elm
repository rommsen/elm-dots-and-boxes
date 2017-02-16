module Player.Types
    exposing
        ( PlayerId
        , PlayerStatus(..)
        , Player
        , PlayerInGame
        , PlayersInGame
        , PlayerForm
        , PlayerPoints
        , Players
        , advance
        , updateCurrent
        , playerInGameFactory
        , playersInGameFactory
        , getWinner
        , addPlayerInGame
        , status
        , player
        , points
        , incrementPoints
        , isPlayerInGame
        , playerIsCurrent
        , numberPlayers
        , previous
        , current
        , next
        , toList
        , validateForm
        , extractFromForm
        , defaultForm
        )

import Dict exposing (Dict)
import Form.Validation exposing (..)
import List.Nonempty


type alias PlayerId =
    String


type alias Player =
    { id : PlayerId
    , name : String
    }


type alias PlayerForm =
    { name : String
    , errors : List Error
    }


defaultForm : PlayerForm
defaultForm =
    PlayerForm "" []


type alias Players =
    Dict PlayerId PlayerInGame


type PlayerInGame
    = PlayerInGame
        { player : Player
        , status : PlayerStatus
        , points : PlayerPoints
        }


type PlayersInGame
    = PlayersInGame
        { previous : List PlayerInGame
        , current : PlayerInGame
        , next : List PlayerInGame
        }


type PlayerStatus
    = Player1
    | Player2
    | Player3
    | Player4
    | Player5


type alias PlayerPoints =
    Int


playerInGameFactory : Player -> PlayerStatus -> PlayerPoints -> PlayerInGame
playerInGameFactory player status points =
    PlayerInGame { player = player, status = status, points = points }


incrementPoints : PlayerInGame -> PlayerInGame
incrementPoints (PlayerInGame { player, status, points }) =
    playerInGameFactory player status (points + 1)


playersInGameToList : PlayersInGame -> List PlayerInGame
playersInGameToList (PlayersInGame { previous, current, next }) =
    previous
        |> (::) current
        |> (++) next


playersInGameFactory : List PlayerInGame -> PlayerInGame -> List PlayerInGame -> PlayersInGame
playersInGameFactory previous current next =
    PlayersInGame { previous = previous, current = current, next = next }


status : PlayerInGame -> PlayerStatus
status (PlayerInGame { status }) =
    status


player : PlayerInGame -> Player
player (PlayerInGame { player }) =
    player


points : PlayerInGame -> PlayerPoints
points (PlayerInGame { points }) =
    points


previous : PlayersInGame -> List PlayerInGame
previous (PlayersInGame { previous }) =
    previous


current : PlayersInGame -> PlayerInGame
current (PlayersInGame { current }) =
    current


next : PlayersInGame -> List PlayerInGame
next (PlayersInGame { next }) =
    next


updateCurrent : PlayersInGame -> PlayerInGame -> PlayersInGame
updateCurrent (PlayersInGame { previous, next }) newCurrent =
    playersInGameFactory previous newCurrent next


addPlayerInGame : PlayersInGame -> PlayerInGame -> PlayersInGame
addPlayerInGame (PlayersInGame { previous, current, next }) player =
    let
        newNext =
            next
                |> List.reverse
                |> (::) player
                |> List.reverse
    in
        playersInGameFactory previous current newNext


isPlayerInGame : Player -> PlayersInGame -> Bool
isPlayerInGame player players =
    playersInGameToList players
        |> List.map (\(PlayerInGame playerInGame) -> playerInGame.player)
        |> List.member player


playerIsCurrent : Player -> PlayersInGame -> Bool
playerIsCurrent localPlayer players =
    localPlayer == (player <| current players)


numberPlayers : PlayersInGame -> Int
numberPlayers players =
    playersInGameToList players
        |> List.length


toList : PlayersInGame -> List PlayerInGame
toList players =
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


advance : PlayersInGame -> PlayersInGame
advance (PlayersInGame { previous, current, next }) =
    let
        {- | Nonempty.List allows me to work without Maybe handling when next is empty

           when it is empty the list being previously "previous" must be the new next
           If I have a single player game, the current player just stays current with
           this implementation
        -}
        previousWithCurrent =
            previous
                |> List.reverse
                |> (::) current
                |> List.reverse
                |> List.Nonempty.fromList
                |> Maybe.withDefault (List.Nonempty.fromElement current)
    in
        case next of
            head :: tail ->
                PlayersInGame
                    { previous = List.Nonempty.toList previousWithCurrent
                    , current = head
                    , next = tail
                    }

            {- | no more next questions available -}
            [] ->
                PlayersInGame
                    { previous = []
                    , current = List.Nonempty.head previousWithCurrent
                    , next = List.Nonempty.tail previousWithCurrent
                    }


validateForm : PlayerForm -> List Error
validateForm form =
    begin form
        |> validate (validateNotBlank "name" << .name)
        |> extractErrors


extractFromForm : PlayerForm -> Maybe Player
extractFromForm form =
    Result.map (Player "") (stringNotBlankResult form.name)
        |> Result.toMaybe
