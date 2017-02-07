module App.Rest exposing (..)

import App.Types exposing (..)
import Exts.Json.Encode as EJE
import Json.Decode as JD
import Json.Decode.Pipeline
import Json.Encode as JE
import Dict


gameDecoder : JD.Decoder Game
gameDecoder =
    Json.Decode.Pipeline.decode Game
        |> Json.Decode.Pipeline.required "id" JD.string
        |> Json.Decode.Pipeline.optional "owner" (JD.nullable playerDecoder) Nothing
        |> Json.Decode.Pipeline.required "boardSize" boardSizeDecoder
        |> Json.Decode.Pipeline.required "boxes" boxesDecoder
        |> Json.Decode.Pipeline.optional "selectedLines" selectedLinesDecoder Dict.empty
        |> Json.Decode.Pipeline.required "status" gameStatusDecoder
        |> Json.Decode.Pipeline.optional "currentPlayer" (JD.nullable JD.string) Nothing
        |> Json.Decode.Pipeline.optional "players" playersDecoder Dict.empty
        |> Json.Decode.Pipeline.hardcoded []


boxesDecoder : JD.Decoder Boxes
boxesDecoder =
    JD.list boxDecoder


boardSizeDecoder : JD.Decoder BoardSize
boardSizeDecoder =
    Json.Decode.Pipeline.decode BoardSize
        |> Json.Decode.Pipeline.required "width" JD.int
        |> Json.Decode.Pipeline.required "height" JD.int


boxDecoder : JD.Decoder Box
boxDecoder =
    Json.Decode.Pipeline.decode Box
        |> Json.Decode.Pipeline.required "up" lineDecoder
        |> Json.Decode.Pipeline.required "down" lineDecoder
        |> Json.Decode.Pipeline.required "left" lineDecoder
        |> Json.Decode.Pipeline.required "right" lineDecoder
        |> Json.Decode.Pipeline.optional "doneBy" (JD.nullable JD.string) Nothing


selectedLinesDecoder : JD.Decoder SelectedLines
selectedLinesDecoder =
    JD.map Dict.fromList (JD.list selectedLineDecoder)


selectedLineDecoder : JD.Decoder ( Line, PlayerId )
selectedLineDecoder =
    JD.map2 (,)
        (JD.index 0 lineDecoder)
        (JD.index 1 JD.string)


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


playerInGameDecoder : JD.Decoder PlayerInGame
playerInGameDecoder =
    Json.Decode.Pipeline.decode createPlayerInGame
        |> Json.Decode.Pipeline.required "player" playerDecoder
        |> Json.Decode.Pipeline.required "status" playerStatusDecoder
        |> Json.Decode.Pipeline.required "points" JD.int


playerDecoder : JD.Decoder Player
playerDecoder =
    Json.Decode.Pipeline.decode Player
        |> Json.Decode.Pipeline.required "id" JD.string
        |> Json.Decode.Pipeline.required "name" JD.string


playerStatusDecoder : JD.Decoder PlayerStatus
playerStatusDecoder =
    JD.string
        |> JD.andThen playerStatusStringDecoder


playerStatusStringDecoder : String -> JD.Decoder PlayerStatus
playerStatusStringDecoder string =
    case string of
        "Player1" ->
            JD.succeed Player1

        "Player2" ->
            JD.succeed Player2

        "Pending" ->
            JD.succeed Pending

        _ ->
            JD.fail "player not available"


playersDecoder : JD.Decoder Players
playersDecoder =
    JD.map Dict.fromList (JD.list playerInGameWithIdAsKeyDecoder)


playerInGameWithIdAsKeyDecoder : JD.Decoder ( PlayerId, PlayerInGame )
playerInGameWithIdAsKeyDecoder =
    JD.map2 (,)
        (JD.index 0 JD.string)
        (JD.index 1 playerInGameDecoder)


gameStatusDecoder : JD.Decoder GameStatus
gameStatusDecoder =
    JD.string
        |> JD.andThen gameStatusStringDecoder


playerPointsDecoder : JD.Decoder PlayerPoints
playerPointsDecoder =
    JD.int


gameStatusStringDecoder : String -> JD.Decoder GameStatus
gameStatusStringDecoder string =
    case string of
        "Open" ->
            JD.succeed Open

        "Running" ->
            JD.succeed Running

        _ ->
            JD.fail "game status not available"


gameEncoder : Game -> JE.Value
gameEncoder game =
    JE.object
        [ ( "id", JE.string game.id )
        , ( "owner", EJE.maybe encodePlayer game.owner )
        , ( "boardSize", boardSizeEncoder game.boardSize )
        , ( "boxes", boxesEncoder game.boxes )
        , ( "selectedLines", selectedLinesEncoder game.selectedLines )
        , ( "status", encodeGameStatus game.status )
        , ( "currentPlayer", EJE.maybe JE.string game.currentPlayer )
        ]


boxesEncoder : Boxes -> JE.Value
boxesEncoder boxes =
    List.map boxEncoder boxes
        |> JE.list


boardSizeEncoder : BoardSize -> JE.Value
boardSizeEncoder { width, height } =
    JE.object
        [ ( "width", JE.int width )
        , ( "height", JE.int height )
        ]


boxEncoder : Box -> JE.Value
boxEncoder box =
    JE.object
        [ ( "up", encodeLine box.up )
        , ( "down", encodeLine box.down )
        , ( "left", encodeLine box.left )
        , ( "right", encodeLine box.right )
        , ( "doneBy", EJE.maybe JE.string box.doneBy )
        ]


selectedLinesEncoder : SelectedLines -> JE.Value
selectedLinesEncoder selectedLines =
    EJE.dict encodeLine JE.string selectedLines


encodeLine : Line -> JE.Value
encodeLine line =
    EJE.tuple2 encodePoint encodePoint line


encodePoint : Point -> JE.Value
encodePoint point =
    EJE.tuple2 JE.int JE.int point


encodePlayerInGame : PlayerInGame -> JE.Value
encodePlayerInGame (PlayerInGame { player, status, points }) =
    JE.object
        [ ( "player", encodePlayer player )
        , ( "status", encodePlayerStatus status )
        , ( "points", JE.int points )
        ]


encodePlayerStatus : PlayerStatus -> JE.Value
encodePlayerStatus playerStatus =
    toString playerStatus
        |> JE.string


encodeGameStatus : GameStatus -> JE.Value
encodeGameStatus gameStatus =
    toString gameStatus
        |> JE.string


encodePlayer : Player -> JE.Value
encodePlayer player =
    JE.object
        [ ( "id", JE.string player.id )
        , ( "name", JE.string player.name )
        ]
