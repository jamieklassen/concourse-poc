module Commands exposing (buildDecoder, buildStatusDecoder, config, fetchData, fetchJobs, fetchJobsUrl, fetchPipelines, fetchPipelinesUrl, fetchTeams, fetchTeamsUrl, jobDecoder, pipelineDecoder, resultToTask, teamDecoder, toTask)

import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import MockHttp exposing (Endpoint(..))
import Models
import Msgs
import Process
import Task exposing (Task)


fetchData : Cmd Msgs.Msg
fetchData =
    fetchJobs
        |> Task.andThen fetchPipelines
        |> Task.andThen fetchTeams
        |> Task.attempt Msgs.OnFetchData


fetchJobs : Task Http.Error (List Models.Job)
fetchJobs =
    MockHttp.get fetchJobsUrl (JD.list jobDecoder)
        |> toTask config


fetchJobsUrl : String
fetchJobsUrl =
    "/api/v1/jobs"


jobDecoder : JD.Decoder Models.Job
jobDecoder =
    JDP.decode Models.Job
        |> JDP.required "name" JD.string
        |> JDP.required "pipeline_name" JD.string
        |> JDP.required "team_name" JD.string
        |> JDP.required "finished_build" (JD.nullable buildDecoder)


fetchPipelines : List Models.Job -> Task Http.Error (List Models.Pipeline)
fetchPipelines jobs =
    MockHttp.get fetchPipelinesUrl (JD.list pipelineDecoder)
        |> toTask config
        |> Task.map (Models.pipelines jobs)


fetchPipelinesUrl : String
fetchPipelinesUrl =
    "/api/v1/pipelines"


pipelineDecoder : JD.Decoder ( String, String, Bool )
pipelineDecoder =
    JDP.decode (,,)
        |> JDP.required "name" JD.string
        |> JDP.required "team_name" JD.string
        |> JDP.required "paused" JD.bool


buildDecoder : JD.Decoder Models.Build
buildDecoder =
    JDP.decode Models.Build
        |> JDP.required "status" buildStatusDecoder


buildStatusDecoder : JD.Decoder Models.BuildStatus
buildStatusDecoder =
    JD.string |> JD.andThen Models.decodeString


fetchTeams : List Models.Pipeline -> Task Http.Error (List Models.Team)
fetchTeams pipelines =
    MockHttp.get fetchTeamsUrl (JD.list teamDecoder)
        |> toTask config
        |> Task.map (Models.teams pipelines)


fetchTeamsUrl : String
fetchTeamsUrl =
    "/api/v1/teams"


teamDecoder : JD.Decoder String
teamDecoder =
    JD.at [ "name" ] JD.string


config : MockHttp.Config
config =
    MockHttp.config
        [ Get
            { url = fetchPipelinesUrl
            , responseTime = 0
            , response = """
                [ { "name": "pipeline"
                  , "team_name": "main" 
                  , "paused": false
                  }
                ]
              """
            }
        , Get
            { url = fetchTeamsUrl
            , responseTime = 0
            , response = """
                [ { "name": "app-ci-cd" }
                ]
              """
            }
        , Get
            { url = fetchJobsUrl
            , responseTime = 0
            , response = """
                [ { "name": "job1" 
                  , "pipeline_name": "pipeline"
                  , "team_name": "main" 
                  , "finished_build": null
                  }
                , { "name": "job2" 
                  , "pipeline_name": "pipeline"
                  , "team_name": "main" 
                  , "finished_build": 
                    { "status": "succeeded" }
                  }
                ]
              """
            }
        ]


toTask : MockHttp.Config -> MockHttp.Request a -> Task Http.Error a
toTask config request =
    let
        ( result, responseTime ) =
            MockHttp.getResult config request
    in
    Process.sleep responseTime
        |> Task.andThen (always <| resultToTask result)


resultToTask : Result a b -> Task a b
resultToTask result =
    case result of
        Err a ->
            Task.fail a

        Ok a ->
            Task.succeed a
