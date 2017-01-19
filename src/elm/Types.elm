module Types exposing (..)

import App.Types exposing (..)


type alias Model =
    { app : App.Types.Model
    }


type Msg
    = AppMsg App.Types.Msg
