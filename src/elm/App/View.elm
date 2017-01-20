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
                        [ span [ class "nav-item is-brand", onClick Start ]
                            [ text "Elm Dots and Boxes" ]
                        ]
                    ]
                ]
            ]
        ]


viewBody : Model -> Html Msg
viewBody model =
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
        section
            [ class "section" ]
            [ table
                [ class tableClasses ]
                [ viewTableBody model
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
                    , classList
                        [ ( "edge__done edge__done__self", isLineSelected box.right model.selectedLines )
                        ]
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
                    , classList
                        [ ( "edge__done edge__done__self", isLineSelected box.down model.selectedLines )
                        ]
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
                , classList
                    [ ( "edge__done edge__done__self", isLineSelected box.up model.selectedLines )
                    ]
                , onClick <| Select box.up
                ]
                []
            , div
                [ class "edge edge__v"
                , classList
                    [ ( "edge__done edge__done__self", isLineSelected box.left model.selectedLines )
                    ]
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
                [ ( "field-cell__done field-cell__done__self", box.done )
                ]
            ]
            [ div
                [ class "edges" ]
                (default ++ lastOnX ++ lastOnY ++ last)
            ]


isLineSelected : Line -> SelectedLines -> Bool
isLineSelected line selectedLines =
    Dict.member line selectedLines



{--
        , table
            [ class "field-table field-table__3 field-table__w3 field-table__h3" ]
            [ tbody
                []
                [ tr
                    [ class "field-row" ]
                    [ td
                        [ class "field-cell" ]
                        [ div
                            [ class "edges" ]
                            [ div
                                [ class "edge edge__h" ]
                                []
                            , div
                                [ class "edge edge__v" ]
                                []
                            , span
                                [ class "dot dot__l dot__t" ]
                                []
                            ]
                        ]
                    , td
                        [ class "field-cell" ]
                        [ div
                            [ class "edges" ]
                            [ div
                                [ class "edge edge__h" ]
                                []
                            , div
                                [ class "edge edge__v" ]
                                []
                            , span
                                [ class "dot dot__l dot__t" ]
                                []
                            ]
                        ]
                    , td
                        [ class "field-cell" ]
                        [ div
                            [ class "edges" ]
                            [ div
                                [ class "edge edge__h" ]
                                []
                            , div
                                [ class "edge edge__v" ]
                                []
                            , div
                                [ class "edge edge__v edge__v__last" ]
                                []
                            , span
                                [ class "dot dot__l dot__t" ]
                                []
                            , span
                                [ class "dot dot__r dot__t" ]
                                []
                            ]
                        ]
                    ]
                , tr
                    [ class "field-row" ]
                    [ td
                        [ class "field-cell" ]
                        [ div
                            [ class "edges" ]
                            [ div
                                [ class "edge edge__h" ]
                                []
                            , div
                                [ class "edge edge__v" ]
                                []
                            , span
                                [ class "dot dot__l dot__t" ]
                                []
                            ]
                        ]
                    , td
                        [ class "field-cell field-cell__done field-cell__done__rival" ]
                        [ div
                            [ class "edges" ]
                            [ div
                                [ class "edge edge__h edge__done edge__done__self" ]
                                []
                            , div
                                [ class "edge edge__v edge__done edge__done__rival" ]
                                []
                            , span
                                [ class "dot dot__l dot__t" ]
                                []
                            ]
                        ]
                    , td
                        [ class "field-cell field-cell__done field-cell__done__rival" ]
                        [ div
                            [ class "edges" ]
                            [ div
                                [ class "edge edge__h edge__done edge__done__self" ]
                                []
                            , div
                                [ class "edge edge__v edge__done edge__done__rival" ]
                                []
                            , div
                                [ class "edge edge__v edge__v__last edge__done edge__done__self" ]
                                []
                            , span
                                [ class "dot dot__l dot__t" ]
                                []
                            , span
                                [ class "dot dot__r dot__t" ]
                                []
                            ]
                        ]
                    ]
                , tr
                    [ class "field-row" ]
                    [ td
                        [ class "field-cell" ]
                        [ div
                            [ class "edges" ]
                            [ div
                                [ class "edge edge__h" ]
                                []
                            , div
                                [ class "edge edge__v" ]
                                []
                            , div
                                [ class "edge edge__h edge__h__last" ]
                                []
                            , span
                                [ class "dot dot__l dot__t" ]
                                []
                            , span
                                [ class "dot dot__l dot__b" ]
                                []
                            ]
                        ]
                    , td
                        [ class "field-cell field-cell__done field-cell__done__rival" ]
                        [ div
                            [ class "edges" ]
                            [ div
                                [ class "edge edge__h edge__done edge__done__rival" ]
                                []
                            , div
                                [ class "edge edge__v edge__done edge__done__rival" ]
                                []
                            , div
                                [ class "edge edge__h edge__h__last edge__done edge__done__self" ]
                                []
                            , span
                                [ class "dot dot__l dot__t" ]
                                []
                            , span
                                [ class "dot dot__l dot__b" ]
                                []
                            ]
                        ]
                    , td
                        [ class "field-cell field-cell__done field-cell__done__self" ]
                        [ div
                            [ class "edges" ]
                            [ div
                                [ class "edge edge__h edge__done edge__done__rival" ]
                                []
                            , div
                                [ class "edge edge__v edge__done edge__done__rival" ]
                                []
                            , div
                                [ class "edge edge__h edge__h__last edge__done edge__done__rival" ]
                                []
                            , div
                                [ class "edge edge__v edge__v__last edge__done edge__done__self edge__done__last" ]
                                []
                            , span
                                [ class "dot dot__l dot__t" ]
                                []
                            , span
                                [ class "dot dot__l dot__b" ]
                                []
                            , span
                                [ class "dot dot__r dot__t" ]
                                []
                            , span
                                [ class "dot dot__r dot__b" ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
        ]
-}
