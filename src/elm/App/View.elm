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
    case ( model.game, model.localPlayer ) of
        ( Just game, Just localPlayer ) ->
            viewGame game localPlayer

        _ ->
            viewLobby model


viewLobby : Model -> Html Msg
viewLobby model =
    section
        [ class "section" ]
        [ div [ class "container" ]
            [ viewLobbyBody model ]
        ]


viewLobbyBody : Model -> Html Msg
viewLobbyBody model =
    let
        lobbyElements =
            case model.localPlayer of
                Nothing ->
                    [ viewLobbyInfoBox model
                    , viewPlayerForm model.playerForm
                    ]

                Just player ->
                    [ viewLobbyInfoBox model
                    , viewGameForm model.gameForm
                    , viewOpenGameTable model.openGames
                    ]
    in
        lobbyElements
            |> List.map viewLobbyElement
            |> div []


viewLobbyElement : Html Msg -> Html Msg
viewLobbyElement element =
    div [ class "columns" ]
        [ div [ class "column is-half is-offset-one-quarter" ]
            [ element ]
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


viewLobbyInfoBox : Model -> Html Msg
viewLobbyInfoBox model =
    let
        yourName =
            case model.localPlayer of
                Just player ->
                    player.name

                Nothing ->
                    if String.isEmpty model.playerForm.name then
                        "Nobody"
                    else
                        model.playerForm.name

        you =
            yourName
                |> text
                |> viewInfoBoxItem "You"

        openGames =
            model.openGames
                |> List.length
                |> toString
                |> text
                |> viewInfoBoxItem "# Open Games"
    in
        div [ class "column" ]
            [ div [ class "box" ]
                [ nav
                    [ class "level is-mobile" ]
                    [ you
                    , openGames
                    ]
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
        Html.form [ onSubmit CreateGame ]
            [ widthInput
            , heightInput
            , div [ class "control is-grouped" ]
                [ submitButton
                ]
            ]


viewOpenGameTable : List Game -> Html Msg
viewOpenGameTable games =
    table
        [ class "table is-striped " ]
        [ thead []
            [ tr [] [ th [ colspan 4 ] [ text "Open games" ] ]
            , tr []
                [ th [] [ text "owner" ]
                , th [] [ text "board size" ]
                , th [] [ text "created at" ]
                , th [] [ text "action" ]
                ]
            ]
        , List.map viewGameRow games
            |> tbody []
        ]


viewGameRow : Game -> Html Msg
viewGameRow game =
    tr []
        [ td [] [ text game.owner.name ]
        , td [] [ text <| toString game.boardSize.width ++ " x " ++ toString game.boardSize.height ]
        , td [] [ text <| toString game.createdAt ]
        , td []
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


viewGame : Game -> Player -> Html Msg
viewGame game localPlayer =
    viewGameInProcess game localPlayer


viewGameInProcess : Game -> Player -> Html Msg
viewGameInProcess game localPlayer =
    section
        [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ viewGameInfoBox game localPlayer ]
            , div [ class "columns" ]
                [ viewGameBoard game
                , viewGameStats game localPlayer
                ]
            , div [ class "columns" ]
                [ div [ class "column model" ]
                    [ text <| toString game ]
                ]
            ]
        ]


viewGameInfoBox : Game -> Player -> Html Msg
viewGameInfoBox game localPlayer =
    let
        (PlayerInGame { player }) =
            getCurrentPlayer game.players

        status =
            toString game.status
                |> text
                |> viewInfoBoxItem "Status"

        startButton =
            if game.owner == localPlayer && game.status == Open then
                button
                    [ class "button is-primary", onClick StartGame ]
                    [ text "Start Game" ]
                    |> viewInfoBoxItem "Action"
            else
                text ""

        turn =
            if game.status == Running then
                text player.name |> viewInfoBoxItem "Turn"
            else
                text ""

        joinRequests =
            if game.status == Open then
                game.joinRequests
                    |> Dict.size
                    |> toString
                    |> text
                    |> viewInfoBoxItem "# Join Requests"
            else
                text ""

        players =
            game.players
                |> numberPlayers
                |> toString
                |> text
                |> viewInfoBoxItem "# Players"
    in
        div [ class "column" ]
            [ div [ class "box" ]
                [ nav
                    [ class "level is-mobile" ]
                    [ startButton
                    , status
                    , turn
                    , players
                    , joinRequests
                    ]
                ]
            ]


viewInfoBoxItem : String -> Html Msg -> Html Msg
viewInfoBoxItem heading content =
    div
        [ class "level-item has-text-centered" ]
        [ div
            []
            [ p [ class "heading" ] [ text heading ]
            , p [ class "title" ] [ content ]
            ]
        ]


viewGameStats : Game -> Player -> Html Msg
viewGameStats game localPlayer =
    div [ class "column" ]
        [ div [ class "box" ]
            [ viewPlayerTable game.players
            , viewJoinRequestTable game localPlayer
            ]
        ]


viewPlayerTable : PlayersInGame -> Html Msg
viewPlayerTable playersInGame =
    let
        playerList =
            playerListSortedByPlayerPoints playersInGame
                |> List.indexedMap viewPlayer
    in
        table
            [ class "table is-striped is-narrow" ]
            [ thead
                []
                [ tr
                    []
                    [ th [] [ text "#" ]
                    , th [] [ text "Player" ]
                    , th [] [ text "Points" ]
                    ]
                ]
            , tbody [] playerList
            ]


viewPlayer : Int -> PlayerInGame -> Html Msg
viewPlayer pos (PlayerInGame { player, points }) =
    tr []
        [ td [] [ text <| toString (pos + 1) ]
        , td [] [ text player.name ]
        , td [] [ text <| toString points ]
        ]


viewJoinRequestTable : Game -> Player -> Html Msg
viewJoinRequestTable game localPlayer =
    if not <| Dict.isEmpty game.joinRequests then
        table
            [ class "table is-striped is-narrow" ]
            [ thead
                []
                [ tr
                    []
                    [ th [ colspan 2 ] [ text "Join requests" ] ]
                ]
            , tbody []
                (game.joinRequests
                    |> Dict.toList
                    |> List.map (viewJoinRequest game.owner localPlayer)
                )
            ]
    else
        text ""


viewJoinRequest : Player -> Player -> JoinGameRequestEntry -> Html Msg
viewJoinRequest owner localPlayer joinGameRequestEntry =
    let
        acceptButton =
            if owner == localPlayer then
                button
                    [ class "button is-primary is-small"
                    , onClick <| AcceptPlayer joinGameRequestEntry
                    ]
                    [ text "Accept" ]
            else
                text ""
    in
        tr []
            [ td [] [ text <| .name (Tuple.second joinGameRequestEntry) ]
            , td [] [ acceptButton ]
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
    tr [ class "field-row" ]
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
