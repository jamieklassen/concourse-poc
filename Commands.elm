module Commands exposing (..)

import Http
import Models
import Msgs
import RemoteData as RD
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


fetchPipelines : Cmd Msgs.Msg
fetchPipelines =
    Http.get fetchPipelinesUrl (JD.list pipelineDecoder)
        |> RD.sendRequest
        |> Cmd.map Msgs.OnFetchPipelines


fetchPipelinesUrl : String
fetchPipelinesUrl =
    "http://localhost:8080/api/v1/pipelines"


pipelineDecoder : JD.Decoder Models.Pipeline
pipelineDecoder =
    JDP.decode Models.Pipeline
        |> JDP.required "name" JD.string
        |> JDP.required "team_name" JD.string
