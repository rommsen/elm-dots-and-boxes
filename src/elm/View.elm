module View exposing (view)

import App.View
import Types exposing (Model, Msg(..))
import Html exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ Html.map AppMsg (App.View.viewHeader model.app)
        , viewBody model
        , Html.map AppMsg (App.View.viewFooter model.app)
        ]


viewBody : Model -> Html Msg
viewBody model =
    div []
        [ Html.map AppMsg (App.View.viewBody model.app)
        ]
