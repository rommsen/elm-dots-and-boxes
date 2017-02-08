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
                            , game
                                |> gameEncoder
                                |> openGame
                            )

                    Nothing ->
                        ( { model | gameForm = gameForm }, Cmd.none )

        StartGame ->
            case model.game of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    if game.owner == model.currentPlayer then
                        ( model
                        , { game | status = Running }
                            |> gameEncoder
                            |> changeGame
                        )
                    else
                        ( model, Cmd.none )

        Select line ->
            case model.game of
                Nothing ->
                    ( model, Cmd.none )

                Just game ->
                    if lineCanNotBeSelected line game then
                        ( model, Cmd.none )
                    else
                        ( model
                        , game
                            |> selectLine line
                            |> gameEncoder
                            |> changeGame
                        )

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

        RequestToJoinGame gameId ->
            case model.currentPlayer of
                Nothing ->
                    ( model, Cmd.none )

                Just player ->
                    ( model
                    , JoinGameRequest gameId player
                        |> requestToJoinGame
                    )

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
    , currentPlayer = Nothing
    , players = Dict.empty
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


lineCanNotBeSelected : Line -> Game -> Bool
lineCanNotBeSelected line game =
    (game.status /= Running) || (Dict.member line game.selectedLines)


selectLine : Line -> Game -> Game
selectLine line game =
    case game.status of
        Running ->
            case getCurrentPlayer game of
                Nothing ->
                    game

                Just (PlayerInGame { player }) ->
                    let
                        newGame =
                            game
                                |> insertLine player line
                                |> updateBoxes player
                                |> updatePlayers player game
                    in
                        if gameHasFinished newGame.boxes then
                            { newGame | status = getWinner game }
                        else if boxWasFinished game newGame then
                            newGame
                        else
                            { newGame | currentPlayer = switchPlayers game }

        _ ->
            game


insertLine : Player -> Line -> Game -> Game
insertLine player line game =
    { game
        | selectedLines = Dict.insert line player.id game.selectedLines
    }


updateBoxes : Player -> Game -> Game
updateBoxes player game =
    { game
        | boxes =
            game.boxes
                |> List.map (updateBox player game.selectedLines)
    }


updateBox : Player -> SelectedLines -> Box -> Box
updateBox player selectedLines box =
    case box.doneBy of
        {- | the box can only be done, when it was not done before -}
        Nothing ->
            let
                doneBy =
                    if boxIsDone box selectedLines then
                        Just player.id
                    else
                        Nothing
            in
                { box | doneBy = doneBy }

        _ ->
            box


updatePlayers : Player -> Game -> Game -> Game
updatePlayers player oldGame newGame =
    { newGame
        | players =
            Dict.update player.id (updatePlayerPoints oldGame newGame) oldGame.players
    }


updatePlayerPoints : Game -> Game -> Maybe PlayerInGame -> Maybe PlayerInGame
updatePlayerPoints oldGame newGame =
    boxWasFinished oldGame newGame
        |> increasePlayerPoints
        |> Maybe.map


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


getCurrentPlayer : Game -> Maybe PlayerInGame
getCurrentPlayer game =
    case game.currentPlayer of
        Nothing ->
            Nothing

        Just playerId ->
            Dict.get playerId game.players


getWinner : Game -> GameStatus
getWinner game =
    Winner Player1



-- if Tuple.first playerPoints > Tuple.second playerPoints then
--     Winner Player1
-- else if Tuple.second playerPoints > Tuple.first playerPoints then
--     Winner Player2
-- else
--     Draw


switchPlayers : Game -> Maybe PlayerId
switchPlayers game =
    Just "abc"



-- if currentPlayer == Player1 then
--     Player2
-- else
--     Player1


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
        ]


port requestToJoinGame : JoinGameRequest -> Cmd msg


port openGame : JE.Value -> Cmd msg


port changeGame : JE.Value -> Cmd msg


port gameOpened : (String -> msg) -> Sub msg


port gameChanged : (JD.Value -> msg) -> Sub msg


port registerPlayer : Player -> Cmd msg


port playerRegistered : (Player -> msg) -> Sub msg


port openGameAdded : (JD.Value -> msg) -> Sub msg
