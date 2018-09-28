module Views exposing (..)

import Html
import Models
import Msgs
import RemoteData as RD


maybeList : RD.WebData (List Models.Pipeline) -> Html.Html Msgs.Msg
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


list : List Models.Pipeline -> Html.Html Msgs.Msg
list pipelines =
    Html.ul [] (List.map (\p -> Html.li [] [ Html.text p.name ]) pipelines)
