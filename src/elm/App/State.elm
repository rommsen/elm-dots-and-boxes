module App.State exposing (init, update, subscriptions)

import App.Types exposing (..)


initialModel : Model
initialModel =
    { query = ""
    , width = 0
    , height = 0
    , grid = []
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
                    5

                width =
                    4

                grid =
                    buildGrid height width

                _ =
                    Debug.log "grid" (toString grid)
            in
                ( { model
                    | grid = grid
                    , width = width
                    , height = height
                  }
                , Cmd.none
                )


buildGrid : Int -> Int -> List (List Box)
buildGrid height width =
    rows (List.range 0 (height - 1)) (List.range 0 (width - 1))


rows : List Int -> List Int -> List (List Box)
rows ys xs =
    List.foldr (\y grid -> row y xs :: grid) [] ys


row : Int -> List Int -> List Box
row y xs =
    List.foldr (\x boxes -> (buildBox x y) :: boxes) [] xs



-- buildBox : Int -> Int -> Box
-- buildBox x y =
--     Dict.singleton ( x, y, x + 1, y ) False
--         |> Dict.insert ( x, y + 1, x, y ) False
--         |> Dict.insert ( x + 1, y, x + 1, y + 1 ) False
--         |> Dict.insert ( x + 1, y + 1, x, y + 1 ) False
--         |> Box


buildBox : Int -> Int -> Box
buildBox x y =
    Box
        ( x, y, x + 1, y )
        ( x + 1, y, x + 1, y + 1 )
        ( x + 1, y + 1, x, y + 1 )
        ( x, y + 1, x, y )



-- pathes x y =
-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.none
