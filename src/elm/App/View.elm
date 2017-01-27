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
    case model.game.status of
        NotStarted ->
            section
                [ class "section" ]
                [ div [ class "container" ]
                    [ div [ class "columns" ]
                        [ div [ class "column" ]
                            [ button
                                [ class "button is-primary"
                                , onClick StartGame
                                ]
                                [ text "Start Game"
                                ]
                            ]
                        ]
                    ]
                ]

        Winner player ->
            viewGameInProcess model

        Draw ->
            viewGameInProcess model

        Process ->
            viewGameInProcess model


viewGameInProcess : Model -> Html Msg
viewGameInProcess model =
    section
        [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ viewGameBoard model
                , viewGameStats model
                ]
            , div [ class "columns" ]
                [ div [ class "column model" ]
                    [ text <| toString model
                    ]
                ]
            ]
        ]


viewGameStats : Model -> Html Msg
viewGameStats model =
    div [ class "column" ]
        [ div [ class "box" ]
            [ table
                [ class "table is-striped is-narrow" ]
                [ thead
                    []
                    [ tr
                        []
                        [ th
                            [ colspan 2 ]
                            [ model.game.status
                                |> toString
                                |> text
                            ]
                        ]
                    ]
                , tbody
                    []
                    [ tr
                        []
                        [ td
                            []
                            [ text "Turn" ]
                        , td
                            []
                            [ text <| toString model.game.currentPlayer ]
                        ]
                    , tr
                        []
                        [ td
                            []
                            [ text "Points" ]
                        , td
                            []
                            [ model.game.playerPoints
                                |> toString
                                |> text
                            ]
                        ]
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
        div [ class "column is-10" ]
            [ div [ class "box" ]
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
            List.Extra.groupsOf width model.game.boxes
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
                    , classList <| lineClasses box.right model.game.selectedLines
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
                    , classList <| lineClasses box.down model.game.selectedLines
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
                , classList <| lineClasses box.up model.game.selectedLines
                , onClick <| Select box.up
                ]
                []
            , div
                [ class "edge edge__v"
                , classList <| lineClasses box.left model.game.selectedLines
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
