module Models exposing (Build, BuildStatus(..), Job, Model, Pipeline, Team, allNames, decodeString, initialModel, pipelines, statusString, teams, toString)

import Http
import Json.Decode as JD
import Set
import Time exposing (Time)


type alias Model =
    { teams : Maybe (Result Http.Error (List Team))
    }


type alias Team =
    { name : String
    , pipelines : List Pipeline
    }


type alias Pipeline =
    { name : String
    , teamName : String
    , paused : Bool
    , jobs : List Job
    }


type alias Job =
    { name : String
    , pipelineName : String
    , teamName : String
    , finishedBuild : Maybe Build
    }


type alias Build =
    { status : BuildStatus
    }


type BuildStatus
    = Succeeded
    | Failed
    | Errored
    | Paused
    | Aborted


initialModel : Model
initialModel =
    { teams = Nothing
    }


pipelines : List Job -> List ( String, String, Bool ) -> List Pipeline
pipelines jobs pairs =
    pairs
        |> List.map
            (\( name, teamName, paused ) ->
                { name = name
                , teamName = teamName
                , paused = paused
                , jobs =
                    jobs
                        |> List.filter
                            (\j ->
                                j.pipelineName == name && j.teamName == teamName
                            )
                }
            )


statusString : Job -> String
statusString =
    .finishedBuild
        >> Maybe.map (.status >> toString >> (++) ": ")
        >> Maybe.withDefault ""


decodeString : String -> JD.Decoder BuildStatus
decodeString string =
    case string of
        "succeeded" ->
            JD.succeed Succeeded

        "failed" ->
            JD.succeed Failed

        "errored" ->
            JD.succeed Errored

        "paused" ->
            JD.succeed Paused

        "aborted" ->
            JD.succeed Aborted

        invalid ->
            JD.fail <| "invalid build status" ++ invalid


toString : BuildStatus -> String
toString buildStatus =
    case buildStatus of
        Failed ->
            "failed"

        Succeeded ->
            "succeeded"

        Errored ->
            "errored"

        Paused ->
            "paused"

        Aborted ->
            "aborted"


teams : List Pipeline -> List String -> List Team
teams pipelines names =
    allNames pipelines names
        |> List.map
            (\teamName ->
                { name = teamName
                , pipelines =
                    pipelines
                        |> List.filter (.teamName >> (==) teamName)
                }
            )


allNames : List Pipeline -> List String -> List String
allNames pipelines names =
    Set.union
        (pipelines |> List.map .teamName |> Set.fromList)
        (names |> Set.fromList)
        |> Set.toList
