module Models exposing (Build, BuildStatus(..), Data, Job, Model, Pipeline, PipelineStatus(..), Resource, Team, allNames, createPipelines, createTeams, decodeString, initialModel, jobStatus, lensWithOptional, now, optionalWithLens, pipelines, status, statusChangeTime, teams, transitionTime)

import Http
import Json.Decode as JD
import List.Extra
import Maybe.Extra as ME
import Monocle.Common exposing (..)
import Monocle.Lens as ML
import Monocle.Optional as MO
import Set
import Time exposing (Time)


type alias Model =
    { data : Maybe (Result Http.Error Data)
    , highDensity : Bool
    }


type alias Data =
    { teams : List Team
    , now : Time
    }


now : MO.Optional Model Time
now =
    (ML.Lens .data (\b a -> { a | data = b })
        |> lensWithOptional maybe
    )
        => result
        |> optionalWithLens
            (ML.Lens .now (\b a -> { a | now = b }))


lensWithOptional : MO.Optional b c -> ML.Lens a b -> MO.Optional a c
lensWithOptional inner outer =
    let
        getOption =
            outer.get >> inner.getOption

        set c =
            ML.modify outer (inner.set c)
    in
    MO.Optional getOption set


optionalWithLens : ML.Lens b c -> MO.Optional a b -> MO.Optional a c
optionalWithLens inner outer =
    let
        getOption =
            outer.getOption >> Maybe.map inner.get

        set c =
            MO.modify outer (inner.set c)
    in
    MO.Optional getOption set


teams : Model -> List (MO.Optional Model Team)
teams model =
    case model.data of
        Just (Ok data) ->
            List.range 0 (List.length data.teams)
                |> List.map
                    (\i ->
                        ((ML.Lens .data (\b a -> { a | data = b })
                            |> lensWithOptional maybe
                         )
                            => result
                            |> optionalWithLens
                                (ML.Lens .teams (\b a -> { a | teams = b }))
                        )
                            => list i
                    )

        _ ->
            []


type alias Team =
    { name : String
    , pipelines : List Pipeline
    }


pipelines : Model -> MO.Optional Model Team -> List (MO.Optional Model Pipeline)
pipelines model to =
    case to.getOption model of
        Nothing ->
            []

        Just team ->
            List.range 0 (List.length team.pipelines)
                |> List.map
                    (\i ->
                        (to |> optionalWithLens (ML.Lens .pipelines (\b a -> { a | pipelines = b })))
                            => list i
                    )


type alias Pipeline =
    { name : String
    , teamName : String
    , paused : Bool
    , jobs : List Job
    , resources : List Resource
    }


type alias Job =
    { name : String
    , pipelineName : String
    , teamName : String
    , finishedBuild : Maybe Build
    , transitionBuild : Maybe Build
    }


type alias Build =
    { status : BuildStatus
    , startTime : Time
    }


type BuildStatus
    = Succeeded
    | Failed
    | Errored
    | Paused
    | Aborted


type alias Resource =
    { name : String
    , pipelineName : String
    , teamName : String
    , lastChecked : Time
    , failingToCheck : Bool
    }


initialModel : Model
initialModel =
    { data = Nothing
    , highDensity = True
    }


createPipelines : List Job -> List Resource -> List ( String, String, Bool ) -> List Pipeline
createPipelines jobs resources pairs =
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
                , resources =
                    resources
                        |> List.filter
                            (\r ->
                                r.pipelineName == name && r.teamName == teamName
                            )
                }
            )


jobStatus : Job -> Maybe BuildStatus
jobStatus =
    .finishedBuild
        >> Maybe.map .status


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


createTeams : List Pipeline -> List String -> List Team
createTeams pipelines names =
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


type PipelineStatus
    = Succeeding (Maybe Time)
    | Failing (Maybe Time)
    | Erroring (Maybe Time)
    | Pausing
    | Pending


status : Pipeline -> PipelineStatus
status pipeline =
    if pipeline.paused then
        Pausing

    else if List.all ((==) Succeeded) <| List.filterMap jobStatus pipeline.jobs then
        Succeeding (statusChangeTime pipeline)

    else if List.any ((==) Errored) <| List.filterMap jobStatus pipeline.jobs then
        Erroring (statusChangeTime pipeline)

    else if List.any ((==) Failed) <| List.filterMap jobStatus pipeline.jobs then
        Failing (statusChangeTime pipeline)

    else
        Pending


statusChangeTime : Pipeline -> Maybe Time
statusChangeTime pipeline =
    let
        transitions =
            pipeline.jobs
                |> List.filter (transitionTime >> ME.isJust)
                |> List.sortBy (transitionTime >> Maybe.withDefault 0)
    in
    transitions
        |> List.Extra.dropWhile (jobStatus >> (==) (Just Succeeded))
        |> List.head
        |> Maybe.map Just
        |> Maybe.withDefault (List.Extra.last transitions)
        |> Maybe.map transitionTime
        |> ME.join


transitionTime : Job -> Maybe Time
transitionTime =
    .transitionBuild
        >> Maybe.map .startTime
