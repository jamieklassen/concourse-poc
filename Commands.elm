module Commands exposing (buildDecoder, buildStatusDecoder, config, fetchData, fetchJobs, fetchJobsUrl, fetchPipelines, fetchPipelinesUrl, fetchResources, fetchResourcesUrl, fetchTeams, fetchTeamsUrl, jobDecoder, pipelineDecoder, resourceDecoder, resultToTask, teamDecoder, toTask)

import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import MockHttp exposing (Endpoint(..))
import Models
import Msgs
import Process
import Task exposing (Task)
import Time


fetchData : Cmd Msgs.Msg
fetchData =
    Task.map2 (,) fetchJobs fetchResources
        |> Task.andThen fetchPipelines
        |> Task.andThen fetchTeams
        |> Task.map2 (flip Models.Data) Time.now
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
        |> JDP.required "transition_build" (JD.nullable buildDecoder)


fetchResources : Task Http.Error (List Models.Resource)
fetchResources =
    MockHttp.get fetchResourcesUrl (JD.list resourceDecoder)
        |> toTask config


fetchResourcesUrl : String
fetchResourcesUrl =
    "/api/v1/resources"


resourceDecoder : JD.Decoder Models.Resource
resourceDecoder =
    JDP.decode Models.Resource
        |> JDP.required "name" JD.string
        |> JDP.required "pipeline_name" JD.string
        |> JDP.required "team_name" JD.string
        |> JDP.required "last_checked" JD.float
        |> JDP.optional "failing_to_check" JD.bool False


fetchPipelines : ( List Models.Job, List Models.Resource ) -> Task Http.Error (List Models.Pipeline)
fetchPipelines ( jobs, resources ) =
    MockHttp.get fetchPipelinesUrl (JD.list pipelineDecoder)
        |> toTask config
        |> Task.map (Models.createPipelines jobs resources)


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
        |> JDP.required "start_time" JD.float


buildStatusDecoder : JD.Decoder Models.BuildStatus
buildStatusDecoder =
    JD.string |> JD.andThen Models.decodeString


fetchTeams : List Models.Pipeline -> Task Http.Error (List Models.Team)
fetchTeams pipelines =
    MockHttp.get fetchTeamsUrl (JD.list teamDecoder)
        |> toTask config
        |> Task.map (Models.createTeams pipelines)


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
                  , "transition_build": null
                  }
                , { "name": "job2" 
                  , "pipeline_name": "pipeline"
                  , "team_name": "main" 
                  , "finished_build": 
                    { "start_time": 1538254370
                    , "status": "succeeded"
                    }
                  , "transition_build": 
                    { "start_time": 1538254370
                    , "status": "succeeded"
                    }
                  }
                ]
              """
            }
        , Get
            { url = fetchResourcesUrl
            , responseTime = 0
            , response = """
                [ { "name": "unit-dockerfile"
                  , "pipeline_name": "pipeline"
                  , "team_name": "main"
                  , "last_checked": 1538294458
                  }
                , { "name": "builder"
                  , "pipeline_name": "pipeline"
                  , "team_name": "main"
                  , "last_checked": 1538294451
                  }
                , { "name": "resource-pr"
                  , "pipeline_name": "pipeline"
                  , "team_name": "main"
                  , "type": "pull-request"
                  , "last_checked": 1537798103
                  , "failing_to_check": true
                  , "check_error": "resource script '/opt/resource/check []' failed: exit status 1"
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
