module Views exposing (..)

import Bindata
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


stickyHeaderConfig : StickyHeaderConfig
stickyHeaderConfig =
    { sectionClass = "section"
    , sectionHeaderClass = "section-header"
    , sectionBodyClass = "section-body"
    , pageHeaderClass = "page-header"
    , pageBodyClass = "page-body"
    }


type alias StickyHeaderConfig =
    { sectionClass : String
    , sectionHeaderClass : String
    , sectionBodyClass : String
    , pageHeaderClass : String
    , pageBodyClass : String
    }


view : Models.Model -> Html Msgs.Msg
view model =
    Html.div
        [ HSA.css
            [ Css.fontFamilies [ "Inconsolata", .value Css.monospace ]
            , Css.fontSize (Css.px 12)
            , Css.letterSpacing (Css.em 0.0425)
            ]
        ]
        [ topBar model
        , body model
        ]


topBar : Models.Model -> Html Msgs.Msg
topBar model =
    case model.screenSize of
        Models.Desktop ->
            Html.div [ HSA.class stickyHeaderConfig.pageHeaderClass, HSA.css (topBarStyles ++ navStyles) ]
                [ concourseLogo model
                , searchBar Models.Desktop
                , userMenu model
                ]

        Models.Mobile ->
            Html.div [ HSA.class stickyHeaderConfig.pageHeaderClass, HSA.css topBarStyles ]
                [ Html.div [ HSA.css navStyles ] [ concourseLogo model, userMenu model ]
                , searchBar Models.Mobile
                ]


topBarStyles : List Css.Style
topBarStyles =
    [ Css.backgroundColor (Css.hex "1e1d1d")
    , Css.color (Css.hex "fff")
    , Css.position Css.fixed
    , Css.top Css.zero
    , Css.width (Css.pct 100)
    , Css.zIndex (Css.int 2)
    ]


navStyles : List Css.Style
navStyles =
    [ Css.displayFlex
    , Css.justifyContent Css.spaceBetween
    ]


concourseLogo : Models.Model -> Html Msgs.Msg
concourseLogo model =
    Html.div
        [ HSA.css
            [ Css.backgroundImage Bindata.concourseLogo
            , Css.backgroundPosition Css.center
            , Css.backgroundRepeat Css.noRepeat
            , Css.backgroundSize2 (Css.px 42) (Css.px 42)
            , Css.height (Css.px 54)
            , Css.width (Css.px 54)
            ]
        ]
        []


searchBar : Models.ScreenSize -> Html Msgs.Msg
searchBar screenSize =
    Html.div
        [ HSA.css
            ([ Css.position Css.relative
             , Css.alignSelf Css.center
             ]
                ++ (case screenSize of
                        Models.Mobile ->
                            [ Css.margin (Css.px 16) ]

                        Models.Desktop ->
                            []
                   )
            )
        ]
        [ Html.input
            [ HSA.type_ "text"
            , HSA.placeholder "search"
            , HSA.css
                [ Css.border3 (Css.px 1) Css.solid (Css.hex "504b4b")
                , Css.backgroundImage Bindata.spyglassIcon
                , Css.backgroundPosition2 (Css.px 12) (Css.px 8)
                , Css.backgroundRepeat Css.noRepeat
                , Css.backgroundColor Css.transparent
                , Css.fontSize (Css.em 1.15)
                , Css.fontFamilies [ "Inconsolata", .value Css.monospace ]
                , Css.color (Css.hex "fff")
                , Css.padding2 Css.zero (Css.px 42)
                , Css.height (Css.px 30)
                , (case screenSize of
                    Models.Desktop ->
                        Css.width (Css.px 306)

                    Models.Mobile ->
                        Css.width (Css.pct 100)
                  )
                ]
            ]
            []
        , Html.span
            [ HSA.css
                [ Css.backgroundImage Bindata.closeIcon
                , Css.backgroundPosition2 (Css.px 10) (Css.px 10)
                , Css.backgroundRepeat Css.noRepeat
                , Css.backgroundColor Css.transparent
                , Css.opacity (Css.num 0.2)
                , Css.border Css.zero
                , Css.padding (Css.px 17)
                , Css.position Css.absolute
                , Css.right Css.zero
                , Css.flexGrow (Css.num 1)
                , Css.cursor Css.pointer
                ]
            ]
            []
        ]


userMenu : Models.Model -> Html Msgs.Msg
userMenu model =
    Html.div
        [ HSA.css
            ([ Css.padding2 Css.zero (Css.px 30)
             , Css.overflow Css.hidden
             , Css.textOverflow Css.ellipsis
             , Css.textAlign Css.center
             , Css.lineHeight (Css.px 54)
             ]
                ++ (case model.screenSize of
                        Models.Desktop ->
                            [ Css.borderLeft3 (Css.px 1) Css.solid (Css.hex "3d3c3c") ]

                        Models.Mobile ->
                            []
                   )
            )
        ]
        [ Html.text model.user ]


body : Models.Model -> Html Msgs.Msg
body model =
    Html.div
        [ HSA.class stickyHeaderConfig.pageBodyClass
        , HSA.css
            [ Css.backgroundColor (Css.hex "3d3c3c")
            , Css.color (Css.hex "e6e7e8")
            ]
        ]
        [ maybeList model ]


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
                ((if model.highDensity then
                    []
                  else
                    [ pipelinePausedToggle model po ]
                 )
                    ++ [ Html.text p.name
                       , resourceErrorText p
                       , pipelineStatusText model p
                       ]
                    ++ (if model.highDensity then
                            []
                        else
                            [ Html.ul [] (List.map job p.jobs) ]
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
            Html.button [ HSE.onClick <| Msgs.TogglePipelinePaused po ] [ Html.text "toggle paused" ]


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
