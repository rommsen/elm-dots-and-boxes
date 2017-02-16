module Board.Rest exposing (..)

import Board.Types exposing (..)
import Player.Rest as Player
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Exts.Json.Encode as EJE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE


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
        |> JDP.optional "doneBy" (JD.nullable Player.playerStatusDecoder) Nothing


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
        , ( "doneBy", EJE.maybe Player.playerStatusEncoder box.doneBy )
        ]


encodeLine : Line -> JE.Value
encodeLine line =
    EJE.tuple2 encodePoint encodePoint line


encodePoint : Point -> JE.Value
encodePoint point =
    EJE.tuple2 JE.int JE.int point
