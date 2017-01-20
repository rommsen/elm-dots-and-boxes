module App.State exposing (init, update, subscriptions)

import App.Types exposing (..)
import Dict exposing (Dict)


initialModel : Model
initialModel =
    { boxes = []
    , selectedLines = Dict.empty
    , boardSize = BoardSize 3 3
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

                _ =
                    Debug.log "boxes" (toString boxes)
            in
                ( { model | boxes = boxes }
                , Cmd.none
                )

        Select line ->
            let
                selectedLines =
                    Dict.insert line True model.selectedLines

                newBoxes =
                    updateBoxes selectedLines model.boxes

                _ =
                    Debug.log "selectedLines" (toString selectedLines)
            in
                ( { model | boxes = newBoxes, selectedLines = selectedLines }, Cmd.none )


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
        False


updateBoxes : SelectedLines -> Boxes -> Boxes
updateBoxes paths boxes =
    List.map (updateBox paths) boxes


updateBox : SelectedLines -> Box -> Box
updateBox paths box =
    let
        done =
            Dict.member box.up paths
                && Dict.member box.down paths
                && Dict.member box.left paths
                && Dict.member box.right paths
    in
        { box | done = done }



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.none
