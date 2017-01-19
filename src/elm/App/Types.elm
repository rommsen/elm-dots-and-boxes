module App.Types exposing (..)


type alias Model =
    { query : String
    , width : Int
    , height : Int
    , grid : List (List Box)
    }



-- type Box
--     = Box (Dict Path Bool)


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
