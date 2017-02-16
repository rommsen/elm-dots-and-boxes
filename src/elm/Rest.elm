module Rest exposing (..)

import Board.Types exposing (..)
import Date.Extra.Format
import Dict
import Exts.Json.Decode as EJD
import Exts.Json.Encode as EJE
import Game.Types exposing (..)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Player.Types as Player
    exposing
        ( PlayerId
        , Players
        , PlayerPoints
        , PlayerInGame
        , PlayersInGame
        , Player
        , PlayerStatus(..)
        , PlayerForm
        )
import Exts.Json.Encode as EJE
import Exts.Json.Decode as EJD
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Dict
import Date.Extra.Format


gameDecoder : JD.Decoder Game
gameDecoder =
    JDP.decode Game
        |> JDP.required "id" JD.string
        |> JDP.required "owner" playerDecoder
        |> JDP.required "createdAt" EJD.decodeDate
        |> JDP.required "boardSize" boardSizeDecoder
        |> JDP.required "boxes" boxesDecoder
        |> JDP.optional "selectedLines" selectedLinesDecoder Dict.empty
        |> JDP.required "status" gameStatusDecoder
        |> JDP.required "result" gameResultDecoder
        |> JDP.required "players" playersInGameDecoder
        |> JDP.optional "availablePlayerStatus" (JD.list playerStatusDecoder) []
        |> JDP.optional "joinRequests" joinRequestsDecoder Dict.empty
        |> JDP.optional "spectators" joinRequestsDecoder Dict.empty


boxesDecoder : JD.Decoder Boxes
boxesDecoder =
    JD.list boxDecoder


boardSizeDecoder : JD.Decoder BoardSize
boardSizeDecoder =
    JDP.decode BoardSize
        |> JDP.required "width" JD.int
        |> JDP.required "height" JD.int


boxDecoder : JD.Decoder Box
boxDecoder =
    JDP.decode Box
        |> JDP.required "up" lineDecoder
        |> JDP.required "down" lineDecoder
        |> JDP.required "left" lineDecoder
        |> JDP.required "right" lineDecoder
        |> JDP.optional "doneBy" (JD.nullable playerStatusDecoder) Nothing


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
    JDP.decode Player.playerInGameFactory
        |> JDP.required "player" playerDecoder
        |> JDP.required "status" playerStatusDecoder
        |> JDP.required "points" JD.int


playersInGameDecoder : JD.Decoder PlayersInGame
playersInGameDecoder =
    JDP.decode Player.playersInGameFactory
        |> JDP.optional "previous" (JD.list playerInGameDecoder) []
        |> JDP.required "current" playerInGameDecoder
        |> JDP.optional "next" (JD.list playerInGameDecoder) []


playerDecoder : JD.Decoder Player
playerDecoder =
    JDP.decode Player
        |> JDP.required "id" JD.string
        |> JDP.required "name" JD.string


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

        "Player3" ->
            JD.succeed Player3

        "Player4" ->
            JD.succeed Player4

        "Player5" ->
            JD.succeed Player5

        _ ->
            JD.fail "player not available"



{-
   erstmal in ein objekt, dann in ein union type?
-}


playersDecoder : JD.Decoder Players
playersDecoder =
    JD.map Dict.fromList (JD.list playerInGameWithIdAsKeyDecoder)


playerInGameWithIdAsKeyDecoder : JD.Decoder ( PlayerId, PlayerInGame )
playerInGameWithIdAsKeyDecoder =
    JD.map2 (,)
        (JD.index 0 JD.string)
        (JD.index 1 playerInGameDecoder)


playerPointsDecoder : JD.Decoder PlayerPoints
playerPointsDecoder =
    JD.int


gameResultDecoder : JD.Decoder GameResult
gameResultDecoder =
    JD.index 0 JD.string
        |> JD.andThen gameResultStringDecoder


gameResultStringDecoder : String -> JD.Decoder GameResult
gameResultStringDecoder string =
    case string of
        "None" ->
            JD.succeed None

        "Winner" ->
            JD.map Winner
                (JD.index 1 playerInGameDecoder)

        "Draw" ->
            JD.map Draw
                (JD.index 1 (JD.list playerInGameDecoder))

        _ ->
            JD.fail "game result not available"


gameStatusDecoder : JD.Decoder GameStatus
gameStatusDecoder =
    JD.string
        |> JD.andThen gameStatusStringDecoder


gameStatusStringDecoder : String -> JD.Decoder GameStatus
gameStatusStringDecoder string =
    case string of
        "Open" ->
            JD.succeed Open

        "Running" ->
            JD.succeed Running

        "Finished" ->
            JD.succeed Finished

        _ ->
            JD.fail "game status not available"


gameEncoder : Game -> JE.Value
gameEncoder game =
    JE.object
        [ ( "id", JE.string game.id )
        , ( "owner", encodePlayer game.owner )
        , ( "createdAt", JE.string <| Date.Extra.Format.isoString game.createdAt )
        , ( "boardSize", boardSizeEncoder game.boardSize )
        , ( "boxes", boxesEncoder game.boxes )
        , ( "selectedLines", selectedLinesEncoder game.selectedLines )
        , ( "status", gameStatusEncoder game.status )
        , ( "result", gameResultEncoder game.result )
        , ( "players", playersInGameEncoder game.players )
        , ( "availablePlayerStatus", JE.list <| List.map playerStatusEncoder game.availablePlayerStatus )
        , ( "joinRequests", joinRequestsEncoder game.joinRequests )
        , ( "spectators", joinRequestsEncoder game.spectators )
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
playersInGameEncoder players =
    JE.object
        [ ( "previous", JE.list <| List.map playerInGameEncoder (Player.previous players) )
        , ( "current", playerInGameEncoder (Player.current players) )
        , ( "next", JE.list <| List.map playerInGameEncoder (Player.next players) )
        ]


playerInGameEncoder : PlayerInGame -> JE.Value
playerInGameEncoder player =
    JE.object
        [ ( "player", encodePlayer (Player.player player) )
        , ( "status", playerStatusEncoder (Player.status player) )
        , ( "points", JE.int (Player.points player) )
        ]


playerStatusEncoder : PlayerStatus -> JE.Value
playerStatusEncoder playerStatus =
    toString playerStatus
        |> JE.string


gameStatusEncoder : GameStatus -> JE.Value
gameStatusEncoder gameStatus =
    gameStatus
        |> toString
        |> JE.string


gameResultEncoder : GameResult -> JE.Value
gameResultEncoder result =
    case result of
        None ->
            JE.list [ JE.string "None" ]

        Winner player ->
            JE.list [ JE.string "Winner", playerInGameEncoder player ]

        Draw players ->
            JE.list
                [ JE.string "Draw"
                , players
                    |> List.map playerInGameEncoder
                    |> JE.list
                ]


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
