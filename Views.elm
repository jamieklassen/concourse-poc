module Views exposing (job, list, maybeList, pipeline, resource, statusText, team, toText, view)

import Css
import Date
import Date.Extra.Duration as DED
import Date.Extra.Format as DEF
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HSA
import Html.Styled.Events as HSE
import Http
import Models
import Monocle.Optional as MO
import Msgs
import Time exposing (Time)


view : Models.Model -> Html Msgs.Msg
view model =
    Html.div []
        [ hdToggle model
        , maybeList model
        ]


hdToggle : Models.Model -> Html Msgs.Msg
hdToggle model =
    Html.label []
        [ Html.text "HD view"
        , Html.input
            [ HSA.type_ "checkbox"
            , HSA.checked model.highDensity
            , HSE.onClick Msgs.ToggleHighDensity
            ]
            []
        ]


maybeList : Models.Model -> Html Msgs.Msg
maybeList model =
    case model.data of
        Nothing ->
            Html.text ""

        Just (Ok { teams, now }) ->
            case teams of
                [] ->
                    Html.text "No pipelines have been set"

                ts ->
                    list model (Models.teams model)

        Just (Err error) ->
            Html.text (toString error)


list : Models.Model -> List (MO.Optional Models.Model Models.Team) -> Html Msgs.Msg
list model teams =
    Html.ul [] (List.map (team model) teams)


team : Models.Model -> MO.Optional Models.Model Models.Team -> Html Msgs.Msg
team model to =
    case to.getOption model of
        Nothing ->
            Html.text ""

        Just t ->
            let
                body =
                    case Models.pipelines model to of
                        [] ->
                            [ Html.text "no pipelines" ]

                        ps ->
                            List.map (pipeline model) ps
            in
            Html.li [] [ Html.text t.name, Html.ul [] body ]


pipeline : Models.Model -> MO.Optional Models.Model Models.Pipeline -> Html Msgs.Msg
pipeline model po =
    case po.getOption model of
        Nothing ->
            Html.text ""

        Just p ->
            Html.li []
                ([ Html.text p.name
                 , resourceErrorText p
                 , pipelineStatusText model p
                 ]
                    ++ (if model.highDensity then
                            []

                        else
                            [ pipelinePausedToggle model po, Html.ul [] (List.map job p.jobs) ]
                       )
                )


resourceErrorText : Models.Pipeline -> Html Msgs.Msg
resourceErrorText pipeline =
    if List.any .failingToCheck pipeline.resources then
        Html.span
            [ HSA.css [ palette.orange ] ]
            [ Html.text " [resource error]" ]

    else
        Html.text ""


pipelineStatusText : Models.Model -> Models.Pipeline -> Html Msgs.Msg
pipelineStatusText model =
    Models.status
        >> pipelineStatusToText model


pipelineStatusToText : Models.Model -> Models.PipelineStatus -> Html Msgs.Msg
pipelineStatusToText model ps =
    case ps of
        Models.Succeeding since ->
            Html.span
                [ HSA.css [ palette.green ] ]
                [ Html.text (": succeeding" ++ statusAgeString model since) ]

        Models.Failing since ->
            Html.span
                [ HSA.css [ palette.red ] ]
                [ Html.text (": failing" ++ statusAgeString model since) ]

        Models.Erroring since ->
            Html.span
                [ HSA.css [ palette.orange ] ]
                [ Html.text (": erroring" ++ statusAgeString model since) ]

        Models.Pausing ->
            Html.span
                [ HSA.css [ palette.blue ] ]
                [ Html.text ": paused" ]

        Models.Pending ->
            Html.text ": pending"


palette =
    { red = Css.color (Css.hex "ed4b35")
    , green = Css.color (Css.hex "11c560")
    , blue = Css.color (Css.hex "4a90e2")
    , orange = Css.color (Css.hex "f5a623")
    , brown = Css.color (Css.hex "8b572a")
    }


statusAgeString : Models.Model -> Maybe Time -> String
statusAgeString model since =
    if model.highDensity then
        ""

    else
        Maybe.map2 (\n s -> DED.diff (Date.fromTime n) (Date.fromTime s) |> durationToString |> (++) " for ")
            (.getOption Models.now model)
            since
            |> Maybe.withDefault ""


durationToString : DED.DeltaRecord -> String
durationToString duration =
    toString duration.day ++ "d" ++ toString duration.hour ++ "h" ++ toString duration.minute ++ "m" ++ toString duration.second ++ "s"


pipelinePausedToggle : Models.Model -> MO.Optional Models.Model Models.Pipeline -> Html Msgs.Msg
pipelinePausedToggle model po =
    case po.getOption model of
        Nothing ->
            Html.text ""

        Just pipeline ->
            Html.label []
                [ Html.text " paused"
                , Html.input
                    [ HSA.type_ "checkbox"
                    , HSA.checked pipeline.paused
                    , HSE.onClick <| Msgs.TogglePipelinePaused po
                    ]
                    []
                ]


job : Models.Job -> Html Msgs.Msg
job j =
    Html.li [] [ Html.text j.name, statusText j ]


statusText : Models.Job -> Html Msgs.Msg
statusText =
    Models.jobStatus
        >> Maybe.map toText
        >> Maybe.withDefault (Html.text "")


toText : Models.BuildStatus -> Html Msgs.Msg
toText buildStatus =
    case buildStatus of
        Models.Failed ->
            Html.span
                [ HSA.css [ palette.red ] ]
                [ Html.text ": failed" ]

        Models.Succeeded ->
            Html.span
                [ HSA.css [ palette.green ] ]
                [ Html.text ": succeeded" ]

        Models.Errored ->
            Html.span
                [ HSA.css [ palette.orange ] ]
                [ Html.text ": errored" ]

        Models.Paused ->
            Html.span
                [ HSA.css [ palette.blue ] ]
                [ Html.text ": paused" ]

        Models.Aborted ->
            Html.span
                [ HSA.css [ palette.brown ] ]
                [ Html.text ": aborted" ]


resource : Models.Resource -> Html Msgs.Msg
resource r =
    Html.li [] [ Html.text r.name ]
