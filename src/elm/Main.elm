module Main exposing (main)

import Html
import State exposing (init, update, subscriptions)
import Types exposing (Model, Msg)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
