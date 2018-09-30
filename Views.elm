module Views exposing (list, maybeList)

import Html
import Http
import Models
import Msgs


maybeList : Maybe (Result Http.Error (List Models.Team)) -> Html.Html msg
maybeList response =
    case response of
        Nothing ->
            Html.text ""

        Just (Ok teams) ->
            list teams

        Just (Err error) ->
            Html.text (toString error)


list : List Models.Team -> Html.Html msg
list teams =
    Html.ul [] (List.map team teams)


team : Models.Team -> Html.Html msg
team t =
    let
        body =
            case t.pipelines of
                [] ->
                    [ Html.text "no pipelines" ]

                ps ->
                    List.map pipeline ps
    in
    Html.li [] [ Html.text t.name, Html.ul [] body ]


pipeline : Models.Pipeline -> Html.Html msg
pipeline p =
    Html.li [] [ Html.text p.name, Html.ul [] (List.map job p.jobs) ]


job : Models.Job -> Html.Html msg
job j =
    Html.li [] [ Html.text (j.name ++ Models.statusString j) ]
