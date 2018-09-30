module Main exposing (Msg(..), init, main, subscriptions, update, view)

import Commands
import Html
import Models
import Msgs
import RemoteData as RD
import Views


main : Program Never Models.Model Msgs.Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Models.Model, Cmd Msgs.Msg )
init =
    ( Models.initialModel, Commands.fetchData )


type Msg
    = OnFetchPipelines (RD.WebData (List Models.Pipeline))


update : Msgs.Msg -> Models.Model -> ( Models.Model, Cmd Msgs.Msg )
update msg model =
    case msg of
        Msgs.OnFetchData resp ->
            ( { model | teams = Just resp }, Cmd.none )


view : Models.Model -> Html.Html Msgs.Msg
view model =
    Html.div [] [ Views.maybeList model.teams ]


subscriptions : Models.Model -> Sub Msgs.Msg
subscriptions model =
    Sub.none
