module Main exposing (init, main, subscriptions, update)

import Commands
import Html
import Html.Styled as HS
import Models
import Monocle.Optional as MO
import Msgs
import RemoteData as RD
import Time
import Views


main : Program Never Models.Model Msgs.Msg
main =
    Html.program
        { init = init
        , view = Views.view >> HS.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


init : ( Models.Model, Cmd Msgs.Msg )
init =
    ( Models.initialModel, Commands.fetchData )


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


subscriptions : Models.Model -> Sub Msgs.Msg
subscriptions model =
    Time.every Time.second Msgs.Tick
