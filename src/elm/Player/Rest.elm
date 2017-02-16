module Player.Rest
    exposing
        ( playerInGameDecoder
        , playerStatusEncoder
        , playerStatusDecoder
        , playerDecoder
        , playerEncoder
        , playersInGameDecoder
        , playersInGameEncoder
        , playerInGameEncoder
        )

import Dict
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
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Dict


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
        [ ( "player", playerEncoder (Player.player player) )
        , ( "status", playerStatusEncoder (Player.status player) )
        , ( "points", JE.int (Player.points player) )
        ]


playerStatusEncoder : PlayerStatus -> JE.Value
playerStatusEncoder playerStatus =
    toString playerStatus
        |> JE.string


playersEncoder : List Player -> JE.Value
playersEncoder players =
    List.map playerEncoder players
        |> JE.list


playerEncoder : Player -> JE.Value
playerEncoder player =
    JE.object
        [ ( "id", JE.string player.id )
        , ( "name", JE.string player.name )
        ]
