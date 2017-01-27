port module App.State exposing (init, update, subscriptions)

import App.Types exposing (..)
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline
import Exts.Json.Encode as EJE


initialModel : Model
initialModel =
    { boxes = []
    , selectedLines = selectedLinesDummy
    , boardSize = BoardSize 2 2
    , game = gameDummy
    , gameStatus = NotStarted
    , currentPlayer = Player1
    , playerPoints = ( 0, 0 )
    }


gameDummy : Game
gameDummy =
    { id = ""
    , playerNames = [ "Roman", "Lena" ]
    , boxes = buildBoxes <| BoardSize 2 2
    , selectedLines = selectedLinesDummy
    , status = NotStarted
    , currentPlayer = Player1
    , playerPoints = ( 0, 0 )
    }


selectedLinesDummy : SelectedLines
selectedLinesDummy =
    Dict.empty
        |> Dict.insert ( ( 0, 0 ), ( 0, 1 ) ) Player1
        |> Dict.insert ( ( 0, 1 ), ( 1, 1 ) ) Player2


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            let
                -- selectedLines =
                --     "[[[[0,0],[0,1]],\"Player1\"],[[[0,1],[1,1]],\"Player2\"]]"
                --
                -- selectedLine =
                --     "[[[0,0],[0,1]],\"Player1\"]"
                --
                -- line =
                --     "[[0,0],[0,1]]"
                --
                -- point =
                --     "[0,0]"
                --
                -- decoded =
                --     -- JD.decodeString pointDecoder point
                --     -- JD.decodeString lineDecoder line
                --     -- JD.decodeString selectedLineDecoder selectedLines
                --     -- JD.decodeString (JD.list selectedLineDecoder) selectedLines
                --     JD.decodeString selectedLinesDecoder selectedLines
                --
                -- _ =
                --     Debug.log "decoded" decoded
                game =
                    { gameDummy | status = Process }
            in
                ( model, startGame <| gameEncoder game )

        GameStarted value ->
            case JD.decodeValue gameDecoder value of
                Ok game ->
                    ( { model
                        | boxes = game.boxes
                        , game = game
                        , gameStatus = Process
                      }
                    , Cmd.none
                    )

                Err err ->
                    let
                        _ =
                            Debug.crash err
                    in
                        ( model, Cmd.none )

        Select line ->
            if Dict.member line model.selectedLines then
                ( model, Cmd.none )
            else
                let
                    selectedLines =
                        Dict.insert line model.currentPlayer model.selectedLines

                    newBoxes =
                        updateBoxes model.currentPlayer selectedLines model.boxes

                    newModel =
                        { model
                            | boxes = newBoxes
                            , selectedLines = selectedLines
                        }
                in
                    ( proceedGame newModel, Cmd.none )


buildBoxes : BoardSize -> Boxes
buildBoxes (BoardSize width height) =
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


updateBoxes : Player -> SelectedLines -> Boxes -> Boxes
updateBoxes player paths boxes =
    List.map (updateBox player paths) boxes


updateBox : Player -> SelectedLines -> Box -> Box
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


proceedGame : Model -> Model
proceedGame model =
    case model.gameStatus of
        NotStarted ->
            model

        Winner player ->
            model

        Draw ->
            model

        Process ->
            let
                newPlayerPoints =
                    calculatePlayerPoints model.boxes
            in
                if gameHasFinished model.boxes then
                    { model
                        | gameStatus = getWinner newPlayerPoints
                        , playerPoints = newPlayerPoints
                    }
                else if playerHasFinishedBox newPlayerPoints model.playerPoints then
                    { model
                        | playerPoints = newPlayerPoints
                    }
                else
                    { model
                        | currentPlayer = switchPlayers model.currentPlayer
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
    List.isEmpty <| List.filter (\box -> box.doneBy == Nothing) boxes


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


switchPlayers : Player -> Player
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


gameDecoder : JD.Decoder Game
gameDecoder =
    Json.Decode.Pipeline.decode Game
        |> Json.Decode.Pipeline.required "id" JD.string
        |> Json.Decode.Pipeline.required "playerNames" (JD.list JD.string)
        |> Json.Decode.Pipeline.required "boxes" boxesDecoder
        |> Json.Decode.Pipeline.required "selectedLines" selectedLinesDecoder
        |> Json.Decode.Pipeline.required "status" gameStatusDecoder
        |> Json.Decode.Pipeline.required "currentPlayer" playerDecoder
        |> Json.Decode.Pipeline.required "playerPoints" playerPointsDecoder


boxesDecoder : JD.Decoder Boxes
boxesDecoder =
    JD.list boxDecoder


boxDecoder : JD.Decoder Box
boxDecoder =
    Json.Decode.Pipeline.decode Box
        |> Json.Decode.Pipeline.required "up" lineDecoder
        |> Json.Decode.Pipeline.required "down" lineDecoder
        |> Json.Decode.Pipeline.required "left" lineDecoder
        |> Json.Decode.Pipeline.required "right" lineDecoder
        |> Json.Decode.Pipeline.optional "doneBy" (JD.nullable playerDecoder) Nothing


selectedLinesDecoder : JD.Decoder SelectedLines
selectedLinesDecoder =
    JD.map Dict.fromList (JD.list selectedLineDecoder)


selectedLineDecoder : JD.Decoder ( Line, Player )
selectedLineDecoder =
    JD.map2 (,)
        (JD.index 0 lineDecoder)
        (JD.index 1 playerDecoder)


lineDecoder : JD.Decoder Line
lineDecoder =
    JD.map2 (,)
        (JD.index 0 pointDecoder)
        (JD.index 1 pointDecoder)


pointDecoder : JD.Decoder Point
pointDecoder =
    JD.map2 (,)
        (JD.index 0 JD.int)
        (JD.index 1 JD.int)


playerDecoder : JD.Decoder Player
playerDecoder =
    JD.string
        |> JD.andThen playerStringDecoder


playerStringDecoder : String -> JD.Decoder Player
playerStringDecoder string =
    case string of
        "Player1" ->
            JD.succeed Player1

        "Player2" ->
            JD.succeed Player2

        _ ->
            JD.fail "player not available"


gameStatusDecoder : JD.Decoder GameStatus
gameStatusDecoder =
    JD.string
        |> JD.andThen gameStatusStringDecoder


playerPointsDecoder : JD.Decoder PlayerPoints
playerPointsDecoder =
    JD.map2 (,)
        (JD.index 0 JD.int)
        (JD.index 1 JD.int)


gameStatusStringDecoder : String -> JD.Decoder GameStatus
gameStatusStringDecoder string =
    case string of
        "NotStarted" ->
            JD.succeed NotStarted

        "Process" ->
            JD.succeed Process

        _ ->
            JD.fail "game status not available"


gameEncoder : Game -> JE.Value
gameEncoder game =
    JE.object
        [ ( "id", JE.string game.id )
        , ( "playerNames", JE.list <| List.map JE.string game.playerNames )
        , ( "boxes", boxesEncoder game.boxes )
        , ( "selectedLines", selectedLinesEncoder game.selectedLines )
        , ( "status", encodeGameStatus game.status )
        , ( "currentPlayer", encodePlayer game.currentPlayer )
        , ( "playerPoints", encodePlayerPoints game.playerPoints )
        ]


boxesEncoder : Boxes -> JE.Value
boxesEncoder boxes =
    List.map boxEncoder boxes
        |> JE.list


boxEncoder : Box -> JE.Value
boxEncoder box =
    JE.object
        [ ( "up", encodeLine box.up )
        , ( "down", encodeLine box.down )
        , ( "left", encodeLine box.left )
        , ( "right", encodeLine box.right )
        , ( "doneBy", EJE.maybe encodePlayer box.doneBy )
        ]


selectedLinesEncoder : SelectedLines -> JE.Value
selectedLinesEncoder selectedLines =
    EJE.dict encodeLine encodePlayer selectedLines


encodeLine : Line -> JE.Value
encodeLine line =
    EJE.tuple2 encodePoint encodePoint line


encodePoint : Point -> JE.Value
encodePoint point =
    EJE.tuple2 JE.int JE.int point


encodePlayer : Player -> JE.Value
encodePlayer player =
    toString player
        |> JE.string


encodeGameStatus : GameStatus -> JE.Value
encodeGameStatus gameStatus =
    toString gameStatus
        |> JE.string


encodePlayerPoints : PlayerPoints -> JE.Value
encodePlayerPoints playerPoints =
    EJE.tuple2 JE.int JE.int playerPoints



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ gameStarted GameStarted
        ]


port startGame : JE.Value -> Cmd msg


port gameStarted : (JD.Value -> msg) -> Sub msg
