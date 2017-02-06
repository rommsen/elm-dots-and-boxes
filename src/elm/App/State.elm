port module App.State exposing (init, update, subscriptions)

import App.Rest exposing (..)
import App.Types exposing (..)
import Dict exposing (Dict)
import Form.Validation exposing (..)
import Json.Decode as JD
import Json.Encode as JE


initialModel : Model
initialModel =
    { game = Nothing
    , gameForm = defaultGameForm
    , currentPlayer = Nothing
    , playerForm = defaultPlayerForm
    , openGames = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RegisterCurrentPlayer ->
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
                        ( newModel, registerPlayer player )

                    Nothing ->
                        ( newModel, Cmd.none )

        CurrentPlayerRegistered currentPlayer ->
            ( { model | currentPlayer = Just currentPlayer }, Cmd.none )

        InputPlayerName name ->
            let
                form =
                    model.playerForm

                newForm =
                    { form
                        | name = name
                        , errors = validatePlayerForm form
                    }
            in
                ( { model | playerForm = newForm }, Cmd.none )

        InputWidth width ->
            let
                form =
                    model.gameForm

                newForm =
                    { form
                        | width = width
                        , errors = validateGameForm form
                    }
            in
                ( { model | gameForm = newForm }, Cmd.none )

        InputHeight height ->
            let
                form =
                    model.gameForm

                newGameForm =
                    { form
                        | height = height
                        , errors = validateGameForm form
                    }
            in
                ( { model | gameForm = newGameForm }, Cmd.none )

        OpenGame ->
            let
                form =
                    model.gameForm

                gameForm =
                    { form | errors = validateGameForm form }
            in
                case extractBoardSizeFromForm gameForm of
                    Just boardSize ->
                        let
                            game =
                                buildGame model.currentPlayer boardSize
                        in
                            ( { model | game = Just game, gameForm = gameForm }
                            , openGame <| gameEncoder game
                            )

                    Nothing ->
                        ( { model | gameForm = gameForm }, Cmd.none )

        StartGame ->
            case model.game of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    if game.owner == model.currentPlayer then
                        ( model, changeGame <| gameEncoder { game | status = Running } )
                    else
                        ( model, Cmd.none )

        Select line ->
            case model.game of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    let
                        lineCanNotBeSelected =
                            (game.status /= Running) || (Dict.member line game.selectedLines)
                    in
                        if lineCanNotBeSelected then
                            ( model, Cmd.none )
                        else
                            let
                                selectedLines =
                                    Dict.insert line game.currentPlayer game.selectedLines

                                newBoxes =
                                    updateBoxes game.currentPlayer selectedLines game.boxes

                                newGame =
                                    proceedGame
                                        { game
                                            | boxes = newBoxes
                                            , selectedLines = selectedLines
                                        }
                            in
                                ( model, changeGame <| gameEncoder newGame )

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
                    ( { model | openGames = openGame :: model.openGames }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.crash err
                    in
                        ( model, Cmd.none )

        JoinGame gameId ->
            case model.currentPlayer of
                Nothing ->
                    ( model, Cmd.none )

                Just player ->
                    ( model, requestToJoinGame <| JoinGameRequest gameId player )

        JoinGameRequested request ->
            case model.game of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    if game.id == request.gameId then
                        let
                            newGame =
                                { game | pendingPlayers = request.player :: game.pendingPlayers }
                        in
                            ( model, changeGame <| gameEncoder newGame )
                    else
                        ( model, Cmd.none )

        AcceptPlayer player ->
            ( model, Cmd.none )


buildGame : Maybe Player -> BoardSize -> Game
buildGame owner boardSize =
    { id = ""
    , owner = owner
    , boardSize = boardSize
    , boxes = buildBoxes boardSize
    , selectedLines = Dict.empty
    , status = Open
    , currentPlayer = Player1
    , playerPoints = ( 0, 0 )
    , pendingPlayers = []
    }



-- , players = Dict.singleton playerId (PlayerInGame player Player1)


buildBoxes : BoardSize -> Boxes
buildBoxes { width, height } =
    rows (List.range 0 (height - 1)) (List.range 0 (width - 1))


rows : List Int -> List Int -> List Box
rows ys xs =
    List.foldr (\y boxes -> row y xs ++ boxes) [] ys


row : Int -> List Int -> List Box
row y xs =
    List.foldr (\x boxes -> (buildBox x y) :: boxes) [] xs


buildBox : Int -> Int -> Box
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


updateBoxes : PlayerStatus -> SelectedLines -> Boxes -> Boxes
updateBoxes player paths boxes =
    boxes
        |> List.map (updateBox player paths)


updateBox : PlayerStatus -> SelectedLines -> Box -> Box
updateBox player selectedLines box =
    case box.doneBy of
        Just _ ->
            box

        Nothing ->
            let
                doneBy =
                    if boxIsDone box selectedLines then
                        Just player
                    else
                        Nothing
            in
                { box | doneBy = doneBy }


proceedGame : Game -> Game
proceedGame game =
    case game.status of
        Open ->
            game

        Winner player ->
            game

        Draw ->
            game

        Running ->
            let
                newPlayerPoints =
                    calculatePlayerPoints game.boxes
            in
                if gameHasFinished game.boxes then
                    { game
                        | status = getWinner newPlayerPoints
                        , playerPoints = newPlayerPoints
                    }
                else if playerHasFinishedBox newPlayerPoints game.playerPoints then
                    { game
                        | playerPoints = newPlayerPoints
                    }
                else
                    { game
                        | currentPlayer = switchPlayers game.currentPlayer
                        , playerPoints = newPlayerPoints
                    }


calculatePlayerPoints : Boxes -> PlayerPoints
calculatePlayerPoints boxes =
    List.foldl
        (\box ( player1Points, player2Points ) ->
            case box.doneBy of
                Nothing ->
                    ( player1Points, player2Points )

                Just player ->
                    if player == Player1 then
                        ( player1Points + 1, player2Points )
                    else
                        ( player1Points, player2Points + 1 )
        )
        ( 0, 0 )
        boxes


gameHasFinished : Boxes -> Bool
gameHasFinished boxes =
    boxes
        |> List.filter (\box -> box.doneBy == Nothing)
        |> List.isEmpty


playerHasFinishedBox : PlayerPoints -> PlayerPoints -> Bool
playerHasFinishedBox newPoints oldPoints =
    newPoints /= oldPoints


getWinner : PlayerPoints -> GameStatus
getWinner playerPoints =
    if Tuple.first playerPoints > Tuple.second playerPoints then
        Winner Player1
    else if Tuple.second playerPoints > Tuple.first playerPoints then
        Winner Player2
    else
        Draw


switchPlayers : PlayerStatus -> PlayerStatus
switchPlayers currentPlayer =
    if currentPlayer == Player1 then
        Player2
    else
        Player1


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
        , playerRegistered CurrentPlayerRegistered
        , openGameAdded OpenGameAdded
        , joinGameRequested JoinGameRequested
        ]


port requestToJoinGame : JoinGameRequest -> Cmd msg


port joinGameRequested : (JoinGameRequest -> msg) -> Sub msg


port openGame : JE.Value -> Cmd msg


port changeGame : JE.Value -> Cmd msg


port gameOpened : (String -> msg) -> Sub msg


port gameChanged : (JD.Value -> msg) -> Sub msg


port registerPlayer : Player -> Cmd msg


port playerRegistered : (Player -> msg) -> Sub msg


port openGameAdded : (JD.Value -> msg) -> Sub msg
