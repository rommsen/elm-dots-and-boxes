module App.State exposing (init, update, subscriptions)

import App.Types exposing (..)
import Dict exposing (Dict)


initialModel : Model
initialModel =
    { boxes = []
    , selectedLines = Dict.empty
    , boardSize = BoardSize 2 2
    , game = NotStarted
    , currentPlayer = Player1
    , playerPoints = ( 0, 0 )
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            let
                boxes =
                    buildBoxes model.boardSize
            in
                ( { model | boxes = boxes, game = Process }
                , Cmd.none
                )

        Select line ->
            if Dict.member line model.selectedLines then
                ( model, Cmd.none )
            else
                let
                    selectedLines =
                        Dict.insert line model.currentPlayer model.selectedLines

                    newBoxes =
                        updateBoxes model.currentPlayer selectedLines model.boxes

                    newModel =
                        { model
                            | boxes = newBoxes
                            , selectedLines = selectedLines
                        }
                in
                    ( proceedGame newModel, Cmd.none )


buildBoxes : BoardSize -> Boxes
buildBoxes (BoardSize width height) =
    rows (List.range 0 (height - 1)) (List.range 0 (width - 1))


rows : List Int -> List Int -> List Box
rows ys xs =
    List.foldr (\y boxes -> row y xs ++ boxes) [] ys


row : Int -> List Int -> List Box
row y xs =
    List.foldr (\x boxes -> (buildBox x y) :: boxes) [] xs


buildBox : Int -> Int -> Box
buildBox x y =
    Box
        ( ( x, y )
        , ( x + 1, y )
        )
        ( ( x, y + 1 )
        , ( x + 1, y + 1 )
        )
        ( ( x, y )
        , ( x, y + 1 )
        )
        ( ( x + 1, y )
        , ( x + 1, y + 1 )
        )
        Nothing


updateBoxes : Player -> SelectedLines -> Boxes -> Boxes
updateBoxes player paths boxes =
    List.map (updateBox player paths) boxes


updateBox : Player -> SelectedLines -> Box -> Box
updateBox player selectedLines box =
    case box.doneBy of
        Just _ ->
            box

        Nothing ->
            let
                doneBy =
                    if boxIsDone box selectedLines then
                        Just player
                    else
                        Nothing
            in
                { box | doneBy = doneBy }


proceedGame : Model -> Model
proceedGame model =
    case model.game of
        NotStarted ->
            model

        Winner player ->
            model

        Draw ->
            model

        Process ->
            let
                newPlayerPoints =
                    calculatePlayerPoints model.boxes
            in
                if gameHasFinished model.boxes then
                    { model
                        | game = getWinner newPlayerPoints
                        , playerPoints = newPlayerPoints
                    }
                else if playerHasFinishedBox newPlayerPoints model.playerPoints then
                    { model
                        | playerPoints = newPlayerPoints
                    }
                else
                    { model
                        | currentPlayer = switchPlayers model.currentPlayer
                        , playerPoints = newPlayerPoints
                    }


calculatePlayerPoints : Boxes -> PlayerPoints
calculatePlayerPoints boxes =
    List.foldl
        (\box ( player1Points, player2Points ) ->
            case box.doneBy of
                Nothing ->
                    ( player1Points, player2Points )

                Just player ->
                    if player == Player1 then
                        ( player1Points + 1, player2Points )
                    else
                        ( player1Points, player2Points + 1 )
        )
        ( 0, 0 )
        boxes


gameHasFinished : Boxes -> Bool
gameHasFinished boxes =
    List.isEmpty <| List.filter (\box -> box.doneBy == Nothing) boxes


playerHasFinishedBox : PlayerPoints -> PlayerPoints -> Bool
playerHasFinishedBox newPoints oldPoints =
    newPoints /= oldPoints


getWinner : PlayerPoints -> Game
getWinner playerPoints =
    if Tuple.first playerPoints > Tuple.second playerPoints then
        Winner Player1
    else if Tuple.second playerPoints > Tuple.first playerPoints then
        Winner Player2
    else
        Draw


switchPlayers : Player -> Player
switchPlayers currentPlayer =
    if currentPlayer == Player1 then
        Player2
    else
        Player1


boxIsDone : Box -> SelectedLines -> Bool
boxIsDone box selectedLines =
    Dict.member box.up selectedLines
        && Dict.member box.down selectedLines
        && Dict.member box.left selectedLines
        && Dict.member box.right selectedLines



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.none
