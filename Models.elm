module Models exposing (..)

import RemoteData as RD


type alias Model =
    { pipelines : RD.WebData (List Pipeline) }


initialModel : Model
initialModel =
    { pipelines = RD.Loading }


type alias Pipeline =
    { name : String
    , teamName : String
    }
