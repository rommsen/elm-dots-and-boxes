module App.View exposing (viewHeader, viewBody)

import App.Types exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit, onInput)
import List.Extra
import Form.Validation exposing (..)
import FormElements exposing (wrapFormElement)


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
        Nothing ->
            viewLobby model

        Just game ->
            viewGame game


viewLobby : Model -> Html Msg
viewLobby model =
    section
        [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ div [ class "column" ]
                    [ viewLobbyBody model
                    ]
                ]
            ]
        ]


viewLobbyBody : Model -> Html Msg
viewLobbyBody model =
    case model.currentPlayer of
        Nothing ->
            viewPlayerForm model.currentPlayerForm

        Just player ->
            viewGameForm model.gameForm


viewPlayerForm : CurrentPlayerForm -> Html Msg
viewPlayerForm form =
    let
        nameError =
            findError "name" form.errors

        nameInput =
            wrapFormElement "Hey, whats your name?" nameError <|
                input
                    [ type_ "text"
                    , classList
                        [ ( "input", True )
                        , ( "is-danger", nameError /= Nothing )
                        ]
                    , onInput InputCurrentPlayerName
                    , placeholder "Name"
                    , value form.name
                    ]
                    []

        submitButton =
            button
                [ type_ "submit"
                , class "button is-primary"
                ]
                [ text <| "Let's go " ++ form.name ]
    in
        Html.form [ onSubmit RegisterCurrentPlayer ]
            [ nameInput
            , div [ class "control is-grouped" ]
                [ submitButton
                ]
            ]


viewGameForm : GameForm -> Html Msg
viewGameForm form =
    let
        widthError =
            findError "width" form.errors

        heightError =
            findError "height" form.errors

        widthInput =
            wrapFormElement "Width" widthError <|
                input
                    [ type_ "text"
                    , classList
                        [ ( "input", True )
                        , ( "is-danger", widthError /= Nothing )
                        ]
                    , onInput InputWidth
                    , placeholder "Width"
                    , value form.width
                    ]
                    []

        heightInput =
            wrapFormElement "Height" heightError <|
                input
                    [ type_ "text"
                    , classList
                        [ ( "input", True )
                        , ( "is-danger", heightError /= Nothing )
                        ]
                    , onInput InputHeight
                    , placeholder "Height"
                    , value form.height
                    ]
                    []

        submitButton =
            button
                [ type_ "submit"
                , class "button is-primary"
                ]
                [ text "Open Game" ]
    in
        Html.form [ onSubmit OpenGame ]
            [ widthInput
            , heightInput
            , div [ class "control is-grouped" ]
                [ submitButton
                ]
            ]


viewGame : Game -> Html Msg
viewGame game =
    case game.status of
        Open ->
            viewGameInProcess game

        Winner player ->
            viewGameInProcess game

        Draw ->
            viewGameInProcess game

        Running ->
            viewGameInProcess game


viewGameInProcess : Game -> Html Msg
viewGameInProcess game =
    section
        [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ viewGameBoard game
                , viewGameStats game
                ]
            , div [ class "columns" ]
                [ div [ class "column model" ]
                    [ text <| toString game
                    ]
                ]
            ]
        ]


viewGameStats : Game -> Html Msg
viewGameStats game =
    let
        startButton =
            if game.status == Open then
                button
                    [ class "button is-primary"
                    , onClick StartGame
                    ]
                    [ text "Start Game"
                    ]
            else
                text ""
    in
        div [ class "column" ]
            [ div [ class "box" ]
                [ table
                    [ class "table is-striped is-narrow" ]
                    [ thead
                        []
                        [ tr
                            []
                            [ th
                                []
                                [ game.status
                                    |> toString
                                    |> text
                                ]
                            , th
                                []
                                [ startButton
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
                                [ text <| toString game.currentPlayer ]
                            ]
                        , tr
                            []
                            [ td
                                []
                                [ text "Points" ]
                            , td
                                []
                                [ game.playerPoints
                                    |> toString
                                    |> text
                                ]
                            ]
                        ]
                    ]
                ]
            ]


viewGameBoard : Game -> Html Msg
viewGameBoard game =
    let
        tableClasses =
            "field-table"
                ++ " field-table__"
                ++ toString game.boardSize.width
                ++ " field-table__w"
                ++ toString game.boardSize.width
                ++ " field-table__"
                ++ toString game.boardSize.height
                ++ " field-table__h"
                ++ toString game.boardSize.height
    in
        div [ class "column is-10" ]
            [ div [ class "box" ]
                [ table
                    [ class tableClasses ]
                    [ viewTableBody game
                    ]
                ]
            ]


viewTableBody : Game -> Html Msg
viewTableBody game =
    let
        grid =
            List.Extra.groupsOf game.boardSize.width game.boxes
    in
        tbody
            []
            (List.indexedMap (viewTableRows game) grid)


viewTableRows : Game -> Int -> List Box -> Html Msg
viewTableRows game y boxes =
    tr
        [ class "field-row" ]
        (List.indexedMap (viewTableCell game y) boxes)


viewTableCell : Game -> Int -> Int -> Box -> Html Msg
viewTableCell game y x box =
    let
        lastOnX =
            if x + 1 == game.boardSize.width then
                [ div
                    [ class "edge edge__v edge__v__last"
                    , classList <| lineClasses box.right game.selectedLines
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
            if y + 1 == game.boardSize.height then
                [ div
                    [ class "edge edge__h edge__h__last"
                    , classList <| lineClasses box.down game.selectedLines
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
            if x + 1 == game.boardSize.width && y + 1 == game.boardSize.height then
                [ span
                    [ class "dot dot__r dot__b" ]
                    []
                ]
            else
                []

        default =
            [ div
                [ class "edge edge__h"
                , classList <| lineClasses box.up game.selectedLines
                , onClick <| Select box.up
                ]
                []
            , div
                [ class "edge edge__v"
                , classList <| lineClasses box.left game.selectedLines
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
