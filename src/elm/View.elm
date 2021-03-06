module View exposing (view)

import Board.Types exposing (..)
import Chat.Types as Chat
import Date
import Date.Extra.Config.Config_en_us
import Date.Extra.Format
import Dict exposing (Dict)
import Form.Elements exposing (wrapFormElement)
import Form.Validation exposing (..)
import Game.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, colspan, href, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List.Extra
import Player.Types as Player exposing (Player, PlayerForm, PlayerInGame, PlayerStatus, PlayersInGame)
import Types exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , viewBody model
        , viewFooter
        ]


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


viewFooter : Html Msg
viewFooter =
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
            viewGame game localPlayer model.turnTimer model.chatInput model.chatMessages

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


viewGameForm : GameForm -> Html Msg
viewGameForm gameForm =
    Html.form [ onSubmit CreateGame ]
        [ h1 [ class "title" ] [ text "Choose game size" ]
        , viewSizeButtons gameForm.boardSize.width InputWidth "Width"
        , viewSizeButtons gameForm.boardSize.height InputHeight "Height"
        , viewTurnTimerButtons gameForm.turnTimer
        , button
            [ type_ "submit"
            , class "button is-primary"
            ]
            [ text "Open new game" ]
        ]


viewSizeButtons : Int -> (Int -> Msg) -> String -> Html Msg
viewSizeButtons modelSize msg inputLabel =
    let
        buttons =
            List.range 2 8
                |> List.map (viewSizeButton modelSize msg)
    in
        div
            [ class "control" ]
            [ label [ class "label" ] [ text inputLabel ]
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


viewTurnTimerButtons : TurnTimer -> Html Msg
viewTurnTimerButtons turnTimerPreset =
    let
        buttons =
            [ 3, 5, 10, 15, 20 ]
                |> List.map
                    (viewTurnTimerButton turnTimerPreset)
    in
        div
            [ class "control" ]
            [ label [ class "label" ] [ text "Turn Timer (in seconds)" ]
            , p
                [ class "control has-addons" ]
                buttons
            ]


viewTurnTimerButton : TurnTimer -> TurnTimer -> Html Msg
viewTurnTimerButton modelTurnTimer turnTimer =
    a
        [ class "button is-info"
        , classList [ ( "is-outlined", turnTimer /= modelTurnTimer ) ]
        , onClick <| InputTurnTimer turnTimer
        ]
        [ text <| toString turnTimer ]


viewGameTable : Dict GameId Game -> String -> Html Msg
viewGameTable games header =
    table
        [ class "table is-striped " ]
        [ thead []
            [ tr [] [ th [ colspan 4 ] [ text header ] ]
            , tr []
                [ th [] [ text "owner" ]
                , th [] [ text "size" ]
                , th [] [ text "timer" ]
                , th [] [ text "created at" ]
                , th [] [ text "action" ]
                ]
            ]
        , games
            |> Dict.values
            |> List.sortWith (\a b -> compare (Date.toTime a.createdAt) (Date.toTime b.createdAt))
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
            , td [] [ text <| toString game.turnTimer ]
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


viewGame : Game -> Player -> TurnTimer -> String -> List Chat.Message -> Html Msg
viewGame game localPlayer turnTimer chatInput chatMessages =
    section
        [ class "section" ]
        [ div [ class "container" ]
            [ div [ class "columns" ]
                [ viewGameInfoBox game localPlayer turnTimer ]
            , div [ class "columns" ]
                [ viewGameBoard game localPlayer
                , viewGameStats game localPlayer
                ]
            , div [ class "columns" ]
                [ viewChatTable chatMessages
                , viewChatForm chatInput
                ]
            ]
        ]


viewGameInfoBox : Game -> Player -> TurnTimer -> Html Msg
viewGameInfoBox game localPlayer turnTimer =
    let
        player =
            game.players
                |> Player.current
                |> Player.player

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
            if
                (not <| Player.isPlayerInGame localPlayer game.players)
                    || (game.status == Finished)
                    || (game.status == Abandoned)
            then
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
                max 0 turnTimer
                    |> viewTurnInfo player
                    |> viewInfoBoxItem "Turn"
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
                |> Player.numberPlayers
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
                    , result
                    , players
                    , joinRequests
                    , spectators
                    , turn
                    ]
                ]
            ]


viewTurnInfo : Player -> Int -> Html Msg
viewTurnInfo player turnTimer =
    div []
        [ text player.name
        , text " "
        , span
            [ class "tag is-medium"
            , classList
                [ ( "is-is-success", turnTimer > 5 )
                , ( "is-warning", turnTimer <= 5 && turnTimer > 2 )
                , ( "is-danger", turnTimer <= 2 )
                ]
            ]
            [ text <| toString turnTimer ]
        ]


viewGameResult : GameResult -> String
viewGameResult result =
    case result of
        None ->
            "None"

        Winner player ->
            "Winner: " ++ viewPlayerResult player

        Draw players ->
            players
                |> List.map viewPlayerResult
                |> List.intersperse ", "
                |> List.foldr (++) ""
                |> (++) "Draw: "


viewPlayerResult : PlayerInGame -> String
viewPlayerResult playerInGame =
    let
        player =
            Player.player playerInGame

        points =
            Player.points playerInGame
    in
        player.name ++ " (" ++ toString points ++ " points)"


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


viewChatTable : List Chat.Message -> Html Msg
viewChatTable chatMessages =
    div [ class "column is-8" ]
        [ div [ class "box" ]
            [ table
                [ class "table is-striped is-narrow" ]
                [ thead
                    []
                    [ tr
                        []
                        [ th [] [ text "Player" ]
                        , th [] [ text "Msg" ]
                        ]
                    ]
                , tbody [] (List.map viewChatMsg chatMessages)
                ]
            ]
        ]


viewChatMsg : Chat.Message -> Html Msg
viewChatMsg { msg, player } =
    tr []
        [ td [] [ text player.name ]
        , td [] [ text msg ]
        ]


viewChatForm : String -> Html Msg
viewChatForm msg =
    div [ class "column" ]
        [ div [ class "box" ]
            [ Html.form [ onSubmit SubmitChatMessage ]
                [ div [ class "control" ]
                    [ p
                        [ class "control" ]
                        [ input
                            [ type_ "text"
                            , class "input"
                            , onInput InputChatMessage
                            , placeholder "Message"
                            , value msg
                            ]
                            []
                        ]
                    ]
                , div [ class "control is-grouped" ]
                    [ button
                        [ type_ "submit"
                        , class "button is-primary"
                        ]
                        [ text "Send " ]
                    ]
                ]
            ]
        ]


viewPlayerTable : PlayersInGame -> Html Msg
viewPlayerTable playersInGame =
    let
        playerList =
            Player.toList playersInGame
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
viewPlayer pos playerInGame =
    let
        player =
            Player.player playerInGame

        points =
            Player.points playerInGame
    in
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


viewGameBoard : Game -> Player -> Html Msg
viewGameBoard game player =
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
                    [ class tableClasses
                    , classList [ ( "game_active", Player.playerIsCurrent player game.players ) ]
                    ]
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
        tbody []
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
