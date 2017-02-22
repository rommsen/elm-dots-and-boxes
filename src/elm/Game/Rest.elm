module Game.Rest exposing (..)

import Board.Types exposing (Line)
import Board.Rest as Board
import Game.Types exposing (..)
import Player.Types exposing (Player, PlayerStatus(..))
import Player.Rest as Player
import Date.Extra.Format
import Dict
import Exts.Json.Decode as EJD
import Exts.Json.Encode as EJE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
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
        |> JDP.required "owner" Player.playerDecoder
        |> JDP.required "createdAt" EJD.decodeDate
        |> JDP.required "boardSize" Board.boardSizeDecoder
        |> JDP.required "turnTimer" JD.int
        |> JDP.required "boxes" Board.boxesDecoder
        |> JDP.optional "selectedLines" selectedLinesDecoder Dict.empty
        |> JDP.required "status" gameStatusDecoder
        |> JDP.required "result" gameResultDecoder
        |> JDP.required "players" Player.playersInGameDecoder
        |> JDP.optional "availablePlayerStatus" (JD.list Player.playerStatusDecoder) []
        |> JDP.optional "joinRequests" joinRequestsDecoder Dict.empty
        |> JDP.optional "spectators" joinRequestsDecoder Dict.empty


selectedLinesDecoder : JD.Decoder SelectedLines
selectedLinesDecoder =
    JD.map Dict.fromList (JD.list selectedLineDecoder)


selectedLineDecoder : JD.Decoder ( Line, PlayerStatus )
selectedLineDecoder =
    JD.map2 (,)
        (JD.index 0 Board.lineDecoder)
        (JD.index 1 Player.playerStatusDecoder)


joinRequestsDecoder : JD.Decoder (Dict.Dict String Player)
joinRequestsDecoder =
    JD.dict Player.playerDecoder


joinRequestDecoder : JD.Decoder ( String, Player )
joinRequestDecoder =
    JD.map2 (,)
        (JD.index 0 JD.string)
        (JD.index 1 Player.playerDecoder)


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
                (JD.index 1 Player.playerInGameDecoder)

        "Draw" ->
            JD.map Draw
                (JD.index 1 (JD.list Player.playerInGameDecoder))

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

        "Abandoned" ->
            JD.succeed Abandoned

        _ ->
            JD.fail "game status not available"


gameEncoder : Game -> JE.Value
gameEncoder game =
    JE.object
        [ ( "id", JE.string game.id )
        , ( "owner", Player.playerEncoder game.owner )
        , ( "createdAt", JE.string <| Date.Extra.Format.isoString game.createdAt )
        , ( "boardSize", Board.boardSizeEncoder game.boardSize )
        , ( "turnTimer", JE.int game.turnTimer )
        , ( "boxes", Board.boxesEncoder game.boxes )
        , ( "selectedLines", selectedLinesEncoder game.selectedLines )
        , ( "status", gameStatusEncoder game.status )
        , ( "result", gameResultEncoder game.result )
        , ( "players", Player.playersInGameEncoder game.players )
        , ( "availablePlayerStatus", JE.list <| List.map Player.playerStatusEncoder game.availablePlayerStatus )
        , ( "joinRequests", joinRequestsEncoder game.joinRequests )
        , ( "spectators", joinRequestsEncoder game.spectators )
        ]


selectedLinesEncoder : SelectedLines -> JE.Value
selectedLinesEncoder selectedLines =
    EJE.dict Board.encodeLine Player.playerStatusEncoder selectedLines


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
            JE.list [ JE.string "Winner", Player.playerInGameEncoder player ]

        Draw players ->
            JE.list
                [ JE.string "Draw"
                , players
                    |> List.map Player.playerInGameEncoder
                    |> JE.list
                ]


joinRequestsEncoder : Dict.Dict String Player -> JE.Value
joinRequestsEncoder joinRequests =
    Dict.toList joinRequests
        |> List.map (Tuple.mapSecond Player.playerEncoder)
        |> JE.object
