port module State exposing (init, subscriptions, update)

import Types exposing (..)
import App.State


init : ( Model, Cmd Msg )
init =
    let
        ( appInitModel, appCmd ) =
            App.State.init

        cmds =
            Cmd.batch
                [ Cmd.map AppMsg appCmd
                ]

        initModel =
            { app = appInitModel
            }
    in
        ( initModel, cmds )


update : Types.Msg -> Model -> ( Model, Cmd Types.Msg )
update msg model =
    case msg of
        AppMsg appMsg ->
            let
                ( appModel, cmd ) =
                    App.State.update appMsg model.app
            in
                ( { model | app = appModel }, Cmd.map AppMsg cmd )


subscriptions : Model -> Sub Types.Msg
subscriptions model =
    Sub.batch
        [ Sub.map AppMsg App.State.subscriptions
        ]
