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
        |> Json.Decode.Pipeline.required "owner" playerDecoder
        |> Json.Decode.Pipeline.required "boardSize" boardSizeDecoder
        |> Json.Decode.Pipeline.required "boxes" boxesDecoder
        |> Json.Decode.Pipeline.optional "selectedLines" selectedLinesDecoder Dict.empty
        |> Json.Decode.Pipeline.required "status" gameStatusDecoder
        |> Json.Decode.Pipeline.required "players" playersInGameDecoder
        |> Json.Decode.Pipeline.optional "availablePlayerStatus" (JD.list playerStatusDecoder) []
        |> Json.Decode.Pipeline.optional "joinRequests" joinRequestsDecoder Dict.empty


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
        |> Json.Decode.Pipeline.optional "doneBy" (JD.nullable playerStatusDecoder) Nothing


selectedLinesDecoder : JD.Decoder SelectedLines
selectedLinesDecoder =
    JD.map Dict.fromList (JD.list selectedLineDecoder)


selectedLineDecoder : JD.Decoder ( Line, PlayerStatus )
selectedLineDecoder =
    JD.map2 (,)
        (JD.index 0 lineDecoder)
        (JD.index 1 playerStatusDecoder)


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


joinRequestsDecoder : JD.Decoder (Dict.Dict String Player)
joinRequestsDecoder =
    JD.dict playerDecoder


joinRequestDecoder : JD.Decoder ( String, Player )
joinRequestDecoder =
    JD.map2 (,)
        (JD.index 0 JD.string)
        (JD.index 1 playerDecoder)


playerInGameDecoder : JD.Decoder PlayerInGame
playerInGameDecoder =
    Json.Decode.Pipeline.decode createPlayerInGame
        |> Json.Decode.Pipeline.required "player" playerDecoder
        |> Json.Decode.Pipeline.required "status" playerStatusDecoder
        |> Json.Decode.Pipeline.required "points" JD.int


playersInGameDecoder : JD.Decoder PlayersInGame
playersInGameDecoder =
    Json.Decode.Pipeline.decode createPlayersInGame
        |> Json.Decode.Pipeline.optional "previous" (JD.list playerInGameDecoder) []
        |> Json.Decode.Pipeline.required "current" playerInGameDecoder
        |> Json.Decode.Pipeline.optional "next" (JD.list playerInGameDecoder) []


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
        , ( "owner", encodePlayer game.owner )
        , ( "boardSize", boardSizeEncoder game.boardSize )
        , ( "boxes", boxesEncoder game.boxes )
        , ( "selectedLines", selectedLinesEncoder game.selectedLines )
        , ( "status", encodeGameStatus game.status )
        , ( "players", playersInGameEncoder game.players )
        , ( "availablePlayerStatus", JE.list <| List.map playerStatusEncoder game.availablePlayerStatus )
        , ( "joinRequests", joinRequestsEncoder game.joinRequests )
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
        , ( "doneBy", EJE.maybe playerStatusEncoder box.doneBy )
        ]


selectedLinesEncoder : SelectedLines -> JE.Value
selectedLinesEncoder selectedLines =
    EJE.dict encodeLine playerStatusEncoder selectedLines


encodeLine : Line -> JE.Value
encodeLine line =
    EJE.tuple2 encodePoint encodePoint line


encodePoint : Point -> JE.Value
encodePoint point =
    EJE.tuple2 JE.int JE.int point


playersInGameEncoder : PlayersInGame -> JE.Value
playersInGameEncoder (PlayersInGame { previous, current, next }) =
    JE.object
        [ ( "previous", JE.list <| List.map playerInGameEncoder previous )
        , ( "current", playerInGameEncoder current )
        , ( "next", JE.list <| List.map playerInGameEncoder next )
        ]


playerInGameEncoder : PlayerInGame -> JE.Value
playerInGameEncoder (PlayerInGame { player, status, points }) =
    JE.object
        [ ( "player", encodePlayer player )
        , ( "status", playerStatusEncoder status )
        , ( "points", JE.int points )
        ]


playerStatusEncoder : PlayerStatus -> JE.Value
playerStatusEncoder playerStatus =
    toString playerStatus
        |> JE.string


encodeGameStatus : GameStatus -> JE.Value
encodeGameStatus gameStatus =
    toString gameStatus
        |> JE.string


playersEncoder : List Player -> JE.Value
playersEncoder players =
    List.map encodePlayer players
        |> JE.list


encodePlayer : Player -> JE.Value
encodePlayer player =
    JE.object
        [ ( "id", JE.string player.id )
        , ( "name", JE.string player.name )
        ]


joinRequestsEncoder : Dict.Dict String Player -> JE.Value
joinRequestsEncoder joinRequests =
    Dict.toList joinRequests
        |> List.map (Tuple.mapSecond encodePlayer)
        |> JE.object



-- |> List.map (Tuple.mapFirst JE.string)
