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
            let
                selectedLines =
                    Dict.insert line model.currentPlayer model.selectedLines

                newBoxes =
                    updateBoxes model.currentPlayer selectedLines model.boxes

                playerPoints =
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
                        newBoxes

                newModel =
                    { model
                        | boxes = newBoxes
                        , selectedLines = selectedLines
                        , playerPoints = playerPoints
                    }
            in
                ( proceedGame model.playerPoints newModel, Cmd.none )


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
updateBox player paths box =
    case box.doneBy of
        Just _ ->
            box

        Nothing ->
            let
                doneBy =
                    if
                        Dict.member box.up paths
                            && Dict.member box.down paths
                            && Dict.member box.left paths
                            && Dict.member box.right paths
                    then
                        Just player
                    else
                        Nothing
            in
                { box | doneBy = doneBy }


proceedGame : PlayerPoints -> Model -> Model
proceedGame playerPoints model =
    case model.game of
        NotStarted ->
            model

        Winner player ->
            model

        Process ->
            if gameHasFinished model.boxes then
                { model | game = Winner <| getWinner model.playerPoints }
            else if playerPoints /= model.playerPoints then
                model
            else
                { model | currentPlayer = switchPlayers model.currentPlayer }


gameHasFinished : Boxes -> Bool
gameHasFinished boxes =
    List.isEmpty <| List.filter (\box -> box.doneBy == Nothing) boxes


getWinner : PlayerPoints -> Player
getWinner playerPoints =
    if Tuple.first playerPoints > Tuple.second playerPoints then
        Player1
    else
        Player2


switchPlayers : Player -> Player
switchPlayers currentPlayer =
    if currentPlayer == Player1 then
        Player2
    else
        Player1



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.none
