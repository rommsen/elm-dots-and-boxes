module Game.Types exposing (..)

import Board.Types exposing (..)
import Date
import Dict exposing (Dict)
import Player.Types as Player
    exposing
        ( PlayerInGame
        , PlayersInGame
        , Player
        , PlayerStatus(..)
        )


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


addNewPlayerToGame : JoinGameRequestEntry -> Game -> Game
addNewPlayerToGame ( joinRequestId, player ) game =
    case game.availablePlayerStatus of
        head :: tail ->
            let
                newPlayers =
                    Player.playerInGameFactory player head 0
                        |> Player.addPlayerInGame game.players
            in
                { game
                    | players = newPlayers
                    , availablePlayerStatus = tail
                    , joinRequests = Dict.remove joinRequestId game.joinRequests
                }

        [] ->
            game


buildGame : Player -> BoardSize -> Date.Date -> Game
buildGame owner boardSize createdAt =
    let
        playerInGame =
            Player.playerInGameFactory owner Player1 0
    in
        { id = ""
        , owner = owner
        , createdAt = createdAt
        , boardSize = boardSize
        , boxes = buildBoxes boardSize
        , selectedLines = Dict.empty
        , status = Open
        , result = None
        , players = Player.playersInGameFactory [] playerInGame []
        , availablePlayerStatus = [ Player2, Player3, Player4, Player5 ]
        , joinRequests = Dict.empty
        , spectators = Dict.empty
        }


countFinishedBoxes : Game -> Int
countFinishedBoxes game =
    game.boxes
        |> List.filter (\box -> box.doneBy /= Nothing)
        |> List.length


evaluateGame : Game -> GameResult
evaluateGame game =
    case Player.getWinner game.players of
        [ winner ] ->
            Winner winner

        winners ->
            Draw winners


selectLine : Line -> Game -> Game
selectLine line oldGame =
    case oldGame.status of
        Running ->
            oldGame
                |> insertLine line
                |> updateBoxes
                |> updatePlayers oldGame
                |> evaluateRound oldGame

        _ ->
            oldGame


evaluateRound : Game -> Game -> Game
evaluateRound oldGame newGame =
    if gameHasFinished newGame.boxes then
        { newGame | status = Finished, result = evaluateGame newGame }
    else if boxWasFinished oldGame newGame then
        newGame
    else
        { newGame | players = Player.advance newGame.players }


insertLine : Line -> Game -> Game
insertLine line game =
    let
        status =
            game.players
                |> Player.current
                |> Player.status
    in
        { game
            | selectedLines = Dict.insert line status game.selectedLines
        }


updateBoxes : Game -> Game
updateBoxes game =
    let
        status =
            game.players
                |> Player.current
                |> Player.status

        newBoxes =
            game.boxes
                |> List.map (updateBox status game.selectedLines)
    in
        { game | boxes = newBoxes }


updateBox : Player.PlayerStatus -> SelectedLines -> Box -> Box
updateBox status selectedLines box =
    case box.doneBy of
        {- | the box can only be done, when it was not done before -}
        Nothing ->
            let
                doneBy =
                    if boxIsDone box selectedLines then
                        Just status
                    else
                        Nothing
            in
                { box | doneBy = doneBy }

        _ ->
            box


updatePlayers : Game -> Game -> Game
updatePlayers oldGame newGame =
    let
        newPlayers =
            Player.current oldGame.players
                |> updatePlayerPoints oldGame newGame
                |> Player.updateCurrent oldGame.players
    in
        { newGame | players = newPlayers }


updatePlayerPoints : Game -> Game -> PlayerInGame -> PlayerInGame
updatePlayerPoints oldGame newGame player =
    if boxWasFinished oldGame newGame then
        Player.incrementPoints player
    else
        player


gameHasFinished : Boxes -> Bool
gameHasFinished boxes =
    boxes
        |> List.filter (\box -> box.doneBy == Nothing)
        |> List.isEmpty


boxWasFinished : Game -> Game -> Bool
boxWasFinished oldGame newGame =
    countFinishedBoxes oldGame /= countFinishedBoxes newGame


boxIsDone : Box -> SelectedLines -> Bool
boxIsDone box selectedLines =
    Dict.member box.up selectedLines
        && Dict.member box.down selectedLines
        && Dict.member box.left selectedLines
        && Dict.member box.right selectedLines


lineCanNotBeSelected : Line -> Player -> Game -> Bool
lineCanNotBeSelected line player game =
    (game.status /= Running)
        || (not <| Player.playerIsCurrent player game.players)
        || (Dict.member line game.selectedLines)
