module App.View exposing (viewHeader, viewBody)

import App.Types exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra


viewHeader : Model -> Html Msg
viewHeader model =
    section [ class "hero is-info" ]
        [ div [ class "hero-head" ]
            [ div [ class "container" ]
                [ div [ class "nav" ]
                    [ div [ class "nav-left" ]
                        [ span [ class "nav-item is-brand" ]
                            [ text "Elm Dots and Boxes" ]
                        ]
                    ]
                ]
            ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    case model.game of
        NotStarted ->
            section
                [ class "section" ]
                [ div [ class "container" ]
                    [ div [ class "columns" ]
                        [ div [ class "column" ]
                            [ button
                                [ class "button is-primary"
                                , onClick Start
                                ]
                                [ text "Start Game"
                                ]
                            ]
                        ]
                    ]
                ]

        Winner player ->
            section
                [ class "section" ]
                [ div [ class "container" ]
                    [ div [ class "columns" ]
                        [ div [ class "column" ]
                            [ h1
                                []
                                [ text <| "Winner: " ++ toString player ]
                            ]
                        ]
                    , viewGameBoard model
                    ]
                ]

        Draw ->
            section
                [ class "section" ]
                [ div [ class "container" ]
                    [ div [ class "columns" ]
                        [ div [ class "column" ]
                            [ h1
                                []
                                [ text "Draw" ]
                            ]
                        ]
                    , viewGameBoard model
                    ]
                ]

        Process ->
            viewGameInProcess model


viewGameInProcess : Model -> Html Msg
viewGameInProcess model =
    section
        [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column" ]
                    [ h1
                        [ classList
                            [ ( "player_active", model.currentPlayer == Player1 )
                            , ( "title", True )
                            ]
                        ]
                        [ text <| "Player1: " ++ toString (Tuple.first model.playerPoints) ]
                    ]
                , div [ class "column" ]
                    [ h1
                        [ classList
                            [ ( "player_active", model.currentPlayer == Player2 )
                            , ( "title", True )
                            ]
                        ]
                        [ text <| "Player2: " ++ toString (Tuple.second model.playerPoints) ]
                    ]
                ]
            , viewGameBoard model
            , div [ class "columns" ]
                [ div [ class "column model" ]
                    [ text <| toString model
                    ]
                ]
            ]
        ]


viewGameBoard : Model -> Html Msg
viewGameBoard model =
    let
        (BoardSize width height) =
            model.boardSize

        tableClasses =
            "field-table"
                ++ " field-table__"
                ++ toString width
                ++ " field-table__w"
                ++ toString width
                ++ " field-table__"
                ++ toString height
                ++ " field-table__h"
                ++ toString height
    in
        div [ class "columns" ]
            [ div [ class "column" ]
                [ table
                    [ class tableClasses ]
                    [ viewTableBody model
                    ]
                ]
            ]


viewTableBody : Model -> Html Msg
viewTableBody model =
    let
        (BoardSize width height) =
            model.boardSize

        grid =
            List.Extra.groupsOf width model.boxes
    in
        tbody
            []
            (List.indexedMap (viewTableRows model) grid)


viewTableRows : Model -> Int -> List Box -> Html Msg
viewTableRows model y boxes =
    tr
        [ class "field-row" ]
        (List.indexedMap (viewTableCell model y) boxes)


viewTableCell : Model -> Int -> Int -> Box -> Html Msg
viewTableCell model y x box =
    let
        (BoardSize width height) =
            model.boardSize

        lastOnX =
            if x + 1 == width then
                [ div
                    [ class "edge edge__v edge__v__last"
                    , classList <| lineClasses box.right model.selectedLines
                    , onClick <| Select box.right
                    ]
                    []
                , span
                    [ class "dot dot__r dot__t" ]
                    []
                ]
            else
                []

        lastOnY =
            if y + 1 == height then
                [ div
                    [ class "edge edge__h edge__h__last"
                    , classList <| lineClasses box.down model.selectedLines
                    , onClick <| Select box.down
                    ]
                    []
                , span
                    [ class "dot dot__l dot__b" ]
                    []
                ]
            else
                []

        last =
            if x + 1 == width && y + 1 == height then
                [ span
                    [ class "dot dot__r dot__b" ]
                    []
                ]
            else
                []

        default =
            [ div
                [ class "edge edge__h"
                , classList <| lineClasses box.up model.selectedLines
                , onClick <| Select box.up
                ]
                []
            , div
                [ class "edge edge__v"
                , classList <| lineClasses box.left model.selectedLines
                , onClick <| Select box.left
                ]
                []
            , span
                [ class "dot dot__l dot__t" ]
                []
            ]
    in
        td
            [ class "field-cell"
            , classList
                [ ( "field-cell__done", box.doneBy /= Nothing )
                , ( "field-cell__done__self", box.doneBy == Just Player1 )
                , ( "field-cell__done__rival", box.doneBy == Just Player2 )
                ]
            ]
            [ div
                [ class "edges" ]
                (default ++ lastOnX ++ lastOnY ++ last)
            ]


lineClasses : Line -> SelectedLines -> List ( String, Bool )
lineClasses line selectedLines =
    let
        player =
            Dict.get line selectedLines
    in
        case player of
            Nothing ->
                []

            Just player ->
                [ ( "edge__done", True )
                , ( "edge__done edge__done__self", player == Player1 )
                , ( "edge__done edge__done__rival", player == Player2 )
                ]
