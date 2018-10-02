port module Main exposing (init, main, subscriptions, update)

import Commands
import Html.Styled as Html
import Models
import Monocle.Optional as MO
import Msgs
import RemoteData as RD
import Time
import Views
import Window


port stickyHeaders : Views.StickyHeaderConfig -> Cmd Msgs.Msg


main : Program Never Models.Model Msgs.Msg
main =
    Html.program
        { init = init
        , view = Views.view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Models.Model, Cmd Msgs.Msg )
init =
    ( Models.initialModel
    , Cmd.batch
        [ Commands.fetchData
        , Task.perform Msgs.SetScreenSize Window.size
        ]
    )


update : Msgs.Msg -> Models.Model -> ( Models.Model, Cmd Msgs.Msg )
update msg model =
    case msg of
        Msgs.OnFetchData resp ->
            ( { model | data = Just resp }, Cmd.none )

        Msgs.ToggleHighDensity ->
            ( { model | highDensity = not model.highDensity }, Cmd.none )

        Msgs.Tick now ->
            ( .set Models.now now model, Cmd.none )

        Msgs.TogglePipelinePaused po ->
            ( MO.modify po (\p -> { p | paused = not p.paused }) model, Cmd.none )

        Msgs.SetScreenSize size ->
            ( { model | screenSize = Models.getScreenSize size }, stickyHeaders Views.stickyHeaderConfig )


subscriptions : Models.Model -> Sub Msgs.Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Msgs.Tick
        , Window.resizes Msgs.SetScreenSize
        ]
