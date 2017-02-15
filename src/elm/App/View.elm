module App.View exposing (viewHeader, viewBody, viewFooter)

import App.Types exposing (..)
import Date
import Date.Extra.Config.Config_en_us
import Date.Extra.Format
import Dict exposing (Dict)
import Form.Validation exposing (..)
import FormElements exposing (wrapFormElement)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
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
                    , div
                        [ class "nav-center" ]
                        [ a
                            [ class "nav-item", href "https://github.com/rommsen/elm-dots-and-boxes" ]
                            [ span [ class "icon" ] [ i [ class "fa fa-github" ] [] ] ]
                        , a
                            [ class "nav-item", href "https://twitter.com/R0MMSEN" ]
                            [ span [ class "icon" ] [ i [ class "fa fa-twitter" ] [] ] ]
                        ]
                    ]
                ]
            ]
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    footer
        [ class "footer" ]
        [ div
            [ class "container" ]
            [ div
                [ class "content has-text-centered" ]
                [ p []
                    [ strong [] [ text "Elm Dots and Boxes" ] ]
                , p []
                    [ text " with "
                    , span
                        [ class "icon s-small" ]
                        [ i [ class "fa fa-heart" ] [] ]
                    , text " by "
                    , a
                        [ href "https://github.com/rommsen" ]
                        [ text "Roman Sachse" ]
                    , text ". The source code is licensed "
                    , a
                        [ href "http://opensource.org/licenses/mit-license.php" ]
                        [ text "MIT" ]
                    , text "."
                    ]
                , p []
                    [ a
                        [ class "icon", href "https://github.com/rommsen/elm-dots-and-boxes" ]
                        [ i [ class "fa fa-github" ] [] ]
                    , a
                        [ class "icon", href "https://twitter.com/R0MMSEN" ]
                        [ i [ class "fa fa-twitter" ] [] ]
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
                    , viewGameForm model.boardSize
                    , viewGameTable model.openGames "Open games"
                    , viewGameTable model.runningGames "Running games"
                    ]
    in
        lobbyElements
            |> List.map viewLobbyElement
            |> div []


viewLobbyElement : Html Msg -> Html Msg
viewLobbyElement element =
    div [ class "columns" ]
        [ div [ class "column is-half is-offset-one-quarter" ]
            [ div [ class "box" ] [ element ] ]
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
                |> Dict.size
                |> toString
                |> text
                |> viewInfoBoxItem "# Open Games"
    in
        nav
            [ class "level" ]
            [ you
            , openGames
            ]


viewGameForm : BoardSize -> Html Msg
viewGameForm { width, height } =
    Html.form [ onSubmit CreateGame ]
        [ h1 [ class "title" ] [ text "Choose game size" ]
        , viewSizeButtons width InputWidth
        , viewSizeButtons height InputHeight
        , button
            [ type_ "submit"
            , class "button is-primary"
            ]
            [ text "Open new game" ]
        ]


viewSizeButtons : Int -> (Int -> Msg) -> Html Msg
viewSizeButtons modelSize msg =
    let
        buttons =
            List.range 2 8
                |> List.map (viewSizeButton modelSize msg)
    in
        div
            [ class "control" ]
            [ label [ class "label" ] [ text "Height" ]
            , p
                [ class "control has-addons" ]
                buttons
            ]


viewSizeButton : Int -> (Int -> Msg) -> Int -> Html Msg
viewSizeButton modelSize msg size =
    a
        [ class "button is-info"
        , classList [ ( "is-outlined", size /= modelSize ) ]
        , onClick <| msg size
        ]
        [ text <| toString size ]


viewGameTable : Dict GameId Game -> String -> Html Msg
viewGameTable games header =
    table
        [ class "table is-striped " ]
        [ thead []
            [ tr [] [ th [ colspan 4 ] [ text header ] ]
            , tr []
                [ th [] [ text "owner" ]
                , th [] [ text "size" ]
                , th [] [ text "created at" ]
                , th [] [ text "action" ]
                ]
            ]
        , games
            |> Dict.values
            |> List.sortBy (\game -> toString game.createdAt)
            |> List.reverse
            |> List.map viewGameRow
            |> tbody []
        ]


viewGameRow : Game -> Html Msg
viewGameRow game =
    let
        joinButton =
            if game.status == Open then
                button
                    [ class "button is-primary is-small"
                    , onClick <| RequestToJoinGame game
                    ]
                    [ text "Join" ]
            else
                text ""
    in
        tr []
            [ td [] [ text game.owner.name ]
            , td [] [ text <| toString game.boardSize.width ++ " x " ++ toString game.boardSize.height ]
            , td [] [ text <| formatDateTime game.createdAt ]
            , td []
                [ joinButton
                , button
                    [ class "button is-info is-small"
                    , onClick <| WatchGame game
                    ]
                    [ text "Watch" ]
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
    section
        [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ viewGameInfoBox game localPlayer ]
            , div [ class "columns" ]
                [ viewGameBoard game
                , viewGameStats game localPlayer
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

        backButton =
            if (not <| playerIsPlayerInGame localPlayer game.players) || game.status == Finished then
                button
                    [ class "button is-primary", onClick BackToLobby ]
                    [ text "Back to Lobby" ]
                    |> viewInfoBoxItem "Action"
            else
                text ""

        result =
            if game.status == Finished then
                viewGameResult game.result
                    |> text
                    |> viewInfoBoxItem "Result"
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

        spectators =
            game.spectators
                |> Dict.size
                |> toString
                |> text
                |> viewInfoBoxItem "# Spectators"

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
                    [ class "level" ]
                    [ startButton
                    , backButton
                    , status
                    , turn
                    , result
                    , players
                    , joinRequests
                    , spectators
                    ]
                ]
            ]


viewGameResult : GameResult -> String
viewGameResult result =
    let
        playerResult (PlayerInGame { player, points }) =
            player.name ++ " (" ++ toString points ++ " points)"
    in
        case result of
            None ->
                "None"

            Winner player ->
                "Winner: " ++ playerResult player

            Draw players ->
                players
                    |> List.map playerResult
                    |> List.intersperse ", "
                    |> List.foldr (++) ""
                    |> (++) "Draw: "



-- |> (++) "Draw: "


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
                    |> List.map (viewJoinRequest game.availablePlayerStatus game.owner localPlayer)
                )
            ]
    else
        text ""


viewJoinRequest : List PlayerStatus -> Player -> Player -> JoinGameRequestEntry -> Html Msg
viewJoinRequest availablePlayerStatus owner localPlayer joinGameRequestEntry =
    let
        acceptButton =
            if List.length availablePlayerStatus > 0 && owner == localPlayer then
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


formatDateTime : Date.Date -> String
formatDateTime date =
    Date.Extra.Format.format Date.Extra.Config.Config_en_us.config Date.Extra.Format.isoDateFormat date
        ++ " "
        ++ Date.Extra.Format.format Date.Extra.Config.Config_en_us.config Date.Extra.Format.isoTimeFormat date
