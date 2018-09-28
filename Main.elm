module Main exposing (..)

import Html
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import RemoteData as RD


type alias Model =
    { pipelines : RD.WebData (List Pipeline) }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchPipelines )


initialModel : Model
initialModel =
    { pipelines = RD.Loading }


type Msg
    = OnFetchPipelines (RD.WebData (List Pipeline))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnFetchPipelines resp ->
            ( { model | pipelines = resp }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div [] [ maybeList model.pipelines ]


maybeList : RD.WebData (List Pipeline) -> Html.Html Msg
maybeList response =
    case response of
        RD.NotAsked ->
            Html.text ""

        RD.Loading ->
            Html.text "Loading..."

        RD.Success pipelines ->
            list pipelines

        RD.Failure error ->
            Html.text (toString error)


list : List Pipeline -> Html.Html Msg
list pipelines =
    Html.ul [] (List.map (\p -> Html.li [] [ Html.text p.name ]) pipelines)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


fetchPipelines : Cmd Msg
fetchPipelines =
    Http.get fetchPipelinesUrl (JD.list pipelineDecoder)
        |> RD.sendRequest
        |> Cmd.map OnFetchPipelines


fetchPipelinesUrl : String
fetchPipelinesUrl =
    "http://localhost:8080/api/v1/pipelines"


pipelineDecoder : JD.Decoder Pipeline
pipelineDecoder =
    JDP.decode Pipeline
        |> JDP.required "name" JD.string
        |> JDP.required "team_name" JD.string


type alias Pipeline =
    { name : String
    , teamName : String
    }
