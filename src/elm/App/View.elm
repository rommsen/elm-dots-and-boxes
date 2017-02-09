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
            [ viewLobbyBody model ]
        ]


viewLobbyBody : Model -> Html Msg
viewLobbyBody model =
    case model.localPlayer of
        Nothing ->
            div [ class "columns" ]
                [ div [ class "column is-half is-offset-one-quarter" ]
                    [ viewPlayerForm model.playerForm
                    ]
                ]

        Just player ->
            div []
                [ div [ class "columns" ]
                    [ div [ class "column is-half is-offset-one-quarter" ]
                        [ viewGameForm model.gameForm
                        ]
                    ]
                , div [ class "columns" ]
                    [ div [ class "column is-half is-offset-one-quarter" ]
                        [ viewGameTable model.openGames
                        ]
                    ]
                ]


viewPlayerForm : PlayerForm -> Html Msg
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
                    , onInput InputPlayerName
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
        Html.form [ onSubmit RegisterLocalPlayer ]
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
                [ text "Open new game" ]
    in
        Html.form [ onSubmit OpenGame ]
            [ widthInput
            , heightInput
            , div [ class "control is-grouped" ]
                [ submitButton
                ]
            ]


viewGameTable : List Game -> Html Msg
viewGameTable games =
    table
        [ class "table is-striped " ]
        [ thead
            []
            [ tr
                []
                [ th
                    [ colspan 2 ]
                    [ text "Open games"
                    ]
                ]
            ]
        , List.map viewGameRow games
            |> tbody []
        ]


viewGameRow : Game -> Html Msg
viewGameRow game =
    tr
        []
        [ td
            []
            [ text <| viewGameDescription game ]
        , td
            []
            [ button
                [ class "button is-primary"
                , onClick <| RequestToJoinGame game
                ]
                [ text "Join" ]
            ]
        ]


viewGameDescription : Game -> String
viewGameDescription game =
    let
        boardSize =
            "Board Size: "
                ++ toString game.boardSize.width
                ++ " x "
                ++ toString game.boardSize.height

        owner =
            "Owner: " ++ game.owner.name
    in
        boardSize ++ " " ++ owner


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
        (PlayerInGame { player }) =
            getCurrentPlayer game.players

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

        rows =
            [ tr
                []
                [ td
                    []
                    [ text "Turn" ]
                , td
                    []
                    [ text <| player.name ]
                ]
            , tr
                []
                [ td
                    []
                    [ text "Points" ]
                , td
                    []
                    []
                ]
            ]
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
                    , tbody [] (rows ++ viewPlayers game)
                    ]
                ]
            ]


viewPlayers : Game -> List (Html Msg)
viewPlayers game =
    game.joinRequests
        |> Dict.toList
        |> List.map (viewPlayer game)


viewPlayer : Game -> JoinGameRequestEntry -> Html Msg
viewPlayer game joinGameRequestEntry =
    tr
        []
        [ td
            []
            [ text <| .name (Tuple.second joinGameRequestEntry) ]
        , td
            []
            [ button
                [ class "button is-primary"
                , onClick <| AcceptPlayer joinGameRequestEntry
                ]
                [ text "Accept"
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


viewTableRows : Game -> Coordinate -> List Box -> Html Msg
viewTableRows game y boxes =
    tr
        [ class "field-row" ]
        (List.indexedMap (viewTableCell game y) boxes)


viewTableCell : Game -> Coordinate -> Coordinate -> Box -> Html Msg
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
            , classList <| boxClasses box
            ]
            [ div
                [ class "edges" ]
                (default ++ lastOnX ++ lastOnY ++ last)
            ]


boxClasses : Box -> List ( String, Bool )
boxClasses box =
    case box.doneBy of
        Nothing ->
            []

        Just playerStatus ->
            [ ( "field-cell__done", True )
            , ( "field-cell__done__" ++ toString playerStatus, True )
            ]


lineClasses : Line -> SelectedLines -> List ( String, Bool )
lineClasses line selectedLines =
    case Dict.get line selectedLines of
        Nothing ->
            []

        Just playerStatus ->
            [ ( "edge__done", True )
            , ( "edge__done edge__done__" ++ toString playerStatus, True )
            ]
