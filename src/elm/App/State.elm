module App.State exposing (init, update, subscriptions)

import App.Types exposing (..)
import Dict exposing (Dict)


initialModel : Model
initialModel =
    { query = ""
    , width = 0
    , height = 0
    , grid = []
    , paths = Dict.empty
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Test ->
            let
                height =
                    2

                width =
                    2

                grid =
                    buildGrid height width

                paths =
                    collectPathes grid

                _ =
                    Debug.log "grid" (toString grid)

                _ =
                    Debug.log "paths" (toString paths)
            in
                ( { model
                    | grid = grid
                    , width = width
                    , height = height
                    , paths = paths
                  }
                , Cmd.none
                )

        Select path ->
            let
                newPaths =
                    Dict.update path
                        (\checked ->
                            case checked of
                                Nothing ->
                                    Nothing

                                Just a ->
                                    Just True
                        )
                        model.paths

                _ =
                    Debug.log "paths" (toString newPaths)
            in
                ( { model | paths = newPaths }, Cmd.none )


buildGrid : Int -> Int -> List (List Box)
buildGrid height width =
    rows (List.range 0 (height - 1)) (List.range 0 (width - 1))


rows : List Int -> List Int -> List (List Box)
rows ys xs =
    List.foldr (\y grid -> row y xs :: grid) [] ys


row : Int -> List Int -> List Box
row y xs =
    List.foldr (\x boxes -> (buildBox x y) :: boxes) [] xs


buildBox : Int -> Int -> Box
buildBox x y =
    Box
        ( x, y, x + 1, y )
        ( x + 1, y, x + 1, y + 1 )
        ( x + 1, y + 1, x, y + 1 )
        ( x, y + 1, x, y )


collectPathes : Grid -> Dict Path Bool
collectPathes grid =
    let
        pathes =
            Dict.empty

        pathList =
            List.foldl (\rows paths -> List.foldl extractPathsFromBox paths rows) Dict.empty grid
    in
        pathList


extractPathsFromBox : Box -> Dict.Dict Path Bool -> Dict.Dict Path Bool
extractPathsFromBox box paths =
    paths
        |> Dict.insert box.up False
        |> Dict.insert box.right False
        |> Dict.insert box.down False
        |> Dict.insert box.left False



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.none
