port module App.State exposing (init, update, subscriptions)

import App.Rest exposing (..)
import App.Types exposing (..)
import Dict exposing (Dict)
import Form.Validation exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import List.Nonempty
import Task
import Date


initialModel : Model
initialModel =
    { game = Nothing
    , gameForm = defaultGameForm
    , localPlayer = Nothing
    , playerForm = defaultPlayerForm
    , openGames = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RegisterLocalPlayer ->
            let
                form =
                    model.playerForm

                playerForm =
                    { form | errors = validatePlayerForm form }

                newModel =
                    { model | playerForm = playerForm }
            in
                case extractPlayerFromForm playerForm of
                    Just player ->
                        ( newModel, registerLocalPlayer player )

                    Nothing ->
                        ( newModel, Cmd.none )

        LocalPlayerRegistered localPlayer ->
            ( { model | localPlayer = Just localPlayer }, Cmd.none )

        InputPlayerName name ->
            let
                form =
                    model.playerForm

                newForm =
                    { form | name = name }

                withErrors =
                    { newForm | errors = validatePlayerForm newForm }
            in
                ( { model | playerForm = withErrors }, Cmd.none )

        InputWidth width ->
            let
                form =
                    model.gameForm

                newForm =
                    { form | width = width }

                withErrors =
                    { newForm | errors = validateGameForm newForm }
            in
                ( { model | gameForm = withErrors }, Cmd.none )

        InputHeight height ->
            let
                form =
                    model.gameForm

                newForm =
                    { form | height = height }

                withErrors =
                    { newForm | errors = validateGameForm newForm }
            in
                ( { model | gameForm = withErrors }, Cmd.none )

        CreateGame ->
            let
                form =
                    model.gameForm

                gameForm =
                    { form | errors = validateGameForm form }
            in
                case ( extractBoardSizeFromForm gameForm, model.localPlayer ) of
                    ( Just boardSize, Just localPlayer ) ->
                        let
                            game =
                                buildGame localPlayer boardSize
                        in
                            ( model
                            , Date.now
                                |> Task.perform (OpenGame localPlayer boardSize)
                            )

                    _ ->
                        ( { model | gameForm = gameForm }, Cmd.none )

        OpenGame localPlayer boardSize openedAt ->
            let
                game =
                    buildGame localPlayer boardSize openedAt
            in
                ( { model | game = Just game }
                , game
                    |> gameEncoder
                    |> openGame
                )

        StartGame ->
            case ( model.game, model.localPlayer ) of
                ( Just game, Just localPlayer ) ->
                    if game.owner == localPlayer then
                        ( model
                        , { game | status = Running }
                            |> gameEncoder
                            |> changeGame
                        )
                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Select line ->
            case ( model.localPlayer, model.game ) of
                ( Just localPlayer, Just game ) ->
                    if lineCanNotBeSelected line localPlayer game then
                        ( model, Cmd.none )
                    else
                        ( model
                        , game
                            |> selectLine line
                            |> gameEncoder
                            |> changeGame
                        )

                _ ->
                    ( model, Cmd.none )

        GameChanged value ->
            case JD.decodeValue gameDecoder value of
                Ok game ->
                    case model.game of
                        Nothing ->
                            ( model, Cmd.none )

                        Just ownGame ->
                            if ownGame.id == game.id then
                                ( { model | game = Just game }, Cmd.none )
                            else
                                ( model, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.crash err
                    in
                        ( model, Cmd.none )

        GameOpened gameId ->
            case model.game of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    let
                        newGame =
                            { game | id = gameId }
                    in
                        ( { model | game = Just newGame }, Cmd.none )

        OpenGameAdded value ->
            case JD.decodeValue gameDecoder value of
                Ok openGame ->
                    ( { model
                        | openGames = openGame :: model.openGames
                      }
                    , Cmd.none
                    )

                Err err ->
                    let
                        _ =
                            Debug.crash err
                    in
                        ( model, Cmd.none )

        RequestToJoinGame game ->
            case model.localPlayer of
                Nothing ->
                    ( model, Cmd.none )

                Just player ->
                    ( { model
                        | game = Just game
                      }
                    , JoinGameRequest game.id player
                        |> requestToJoinGame
                    )

        AcceptPlayer joinGameRequestEntry ->
            case ( model.game, model.localPlayer ) of
                ( Just game, Just localPlayer ) ->
                    if game.owner == localPlayer then
                        ( model
                        , addNewPlayerToGame joinGameRequestEntry game
                            |> gameEncoder
                            |> changeGame
                        )
                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


addNewPlayerToGame : JoinGameRequestEntry -> Game -> Game
addNewPlayerToGame ( joinRequestId, player ) game =
    case game.availablePlayerStatus of
        head :: tail ->
            let
                newPlayers =
                    createPlayerInGame player head 0
                        |> addPlayer game.players
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
            createPlayerInGame owner Player1 0
    in
        { id = ""
        , owner = owner
        , createdAt = createdAt
        , boardSize = boardSize
        , boxes = buildBoxes boardSize
        , selectedLines = Dict.empty
        , status = Open
        , result = None
        , players = createPlayersInGame [] playerInGame []
        , availablePlayerStatus = [ Player2 ]
        , joinRequests = Dict.empty
        }


buildBoxes : BoardSize -> Boxes
buildBoxes { width, height } =
    rows (List.range 0 (height - 1)) (List.range 0 (width - 1))


rows : List Coordinate -> List Coordinate -> List Box
rows ys xs =
    List.foldr (\y boxes -> row y xs ++ boxes) [] ys


row : Coordinate -> List Coordinate -> List Box
row y xs =
    List.foldr (\x boxes -> (buildBox x y) :: boxes) [] xs


buildBox : Coordinate -> Coordinate -> Box
buildBox x y =
    Box
        ( ( x, y )
        , ( x + 1, y )
        )
        ( ( x, y + 1 )
        , ( x + 1, y + 1 )
        )
        ( ( x, y )
        , ( x, y + 1 )
        )
        ( ( x + 1, y )
        , ( x + 1, y + 1 )
        )
        Nothing


lineCanNotBeSelected : Line -> Player -> Game -> Bool
lineCanNotBeSelected line player game =
    (game.status /= Running)
        || (not <| localPlayerIsCurrentPlayer player game.players)
        || (Dict.member line game.selectedLines)


localPlayerIsCurrentPlayer : Player -> PlayersInGame -> Bool
localPlayerIsCurrentPlayer localPlayer (PlayersInGame { current }) =
    let
        (PlayerInGame { player }) =
            current
    in
        localPlayer == player


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
        { newGame | players = nextPlayer newGame.players }


nextPlayer : PlayersInGame -> PlayersInGame
nextPlayer (PlayersInGame { previous, current, next }) =
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


initPlayersInGame : PlayerInGame -> PlayersInGame
initPlayersInGame player =
    PlayersInGame
        { previous = []
        , current = player
        , next = []
        }


insertLine : Line -> Game -> Game
insertLine line game =
    let
        (PlayerInGame { status }) =
            getCurrentPlayer game.players
    in
        { game
            | selectedLines = Dict.insert line status game.selectedLines
        }


updateBoxes : Game -> Game
updateBoxes game =
    let
        (PlayerInGame { status }) =
            getCurrentPlayer game.players

        newBoxes =
            game.boxes
                |> List.map (updateBox status game.selectedLines)
    in
        { game | boxes = newBoxes }


updateBox : PlayerStatus -> SelectedLines -> Box -> Box
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
            getCurrentPlayer oldGame.players
                |> updatePlayerPoints oldGame newGame
                |> updateCurrentPlayer oldGame.players
    in
        { newGame | players = newPlayers }


updatePlayerPoints : Game -> Game -> PlayerInGame -> PlayerInGame
updatePlayerPoints oldGame newGame =
    boxWasFinished oldGame newGame
        |> increasePlayerPoints


increasePlayerPoints : Bool -> PlayerInGame -> PlayerInGame
increasePlayerPoints boxWasFinished (PlayerInGame { player, status, points }) =
    if boxWasFinished then
        createPlayerInGame player status (points + 1)
    else
        createPlayerInGame player status points


gameHasFinished : Boxes -> Bool
gameHasFinished boxes =
    boxes
        |> List.filter (\box -> box.doneBy == Nothing)
        |> List.isEmpty


boxWasFinished : Game -> Game -> Bool
boxWasFinished oldGame newGame =
    countFinishedBoxes oldGame /= countFinishedBoxes newGame


countFinishedBoxes : Game -> Int
countFinishedBoxes game =
    game.boxes
        |> List.filter (\box -> box.doneBy /= Nothing)
        |> List.length


evaluateGame : Game -> GameResult
evaluateGame game =
    case getWinner game.players of
        [ winner ] ->
            Winner winner

        winners ->
            Draw winners


boxIsDone : Box -> SelectedLines -> Bool
boxIsDone box selectedLines =
    Dict.member box.up selectedLines
        && Dict.member box.down selectedLines
        && Dict.member box.left selectedLines
        && Dict.member box.right selectedLines


validateGameForm : GameForm -> List Error
validateGameForm form =
    begin form
        |> validate (validateInt "width" << .width)
        |> validate (validateInt "height" << .width)
        |> extractErrors


extractBoardSizeFromForm : GameForm -> Maybe BoardSize
extractBoardSizeFromForm form =
    Result.map2 BoardSize
        (String.toInt form.width)
        (String.toInt form.height)
        |> Result.toMaybe


validatePlayerForm : PlayerForm -> List Error
validatePlayerForm form =
    begin form
        |> validate (validateNotBlank "name" << .name)
        |> extractErrors


extractPlayerFromForm : PlayerForm -> Maybe Player
extractPlayerFromForm form =
    Result.map (Player "") (stringNotBlankResult form.name)
        |> Result.toMaybe



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ gameOpened GameOpened
        , gameChanged GameChanged
        , localPlayerRegistered LocalPlayerRegistered
        , openGameAdded OpenGameAdded
        ]


port requestToJoinGame : JoinGameRequest -> Cmd msg


port openGame : JE.Value -> Cmd msg


port changeGame : JE.Value -> Cmd msg


port gameOpened : (String -> msg) -> Sub msg


port gameChanged : (JD.Value -> msg) -> Sub msg


port registerLocalPlayer : Player -> Cmd msg


port localPlayerRegistered : (Player -> msg) -> Sub msg


port openGameAdded : (JD.Value -> msg) -> Sub msg
