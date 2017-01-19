module App.Types exposing (..)

import Dict exposing (Dict)


type alias Model =
    { query : String
    , width : Int
    , height : Int
    , grid : Grid
    , paths : Dict Path Bool
    }



-- type Box
--     = Box (Dict Path Bool)


type alias Grid =
    List (List Box)


type alias Path =
    ( Int, Int, Int, Int )



-- type alias Boing =
--     { edges : List Path
--       done: Bool
--       doneBy: Player
--     }


type alias Box =
    { up : Path
    , right : Path
    , down : Path
    , left : Path
    }


type Pathes
    = Dict Path Bool


type Msg
    = Test
    | Select Path
