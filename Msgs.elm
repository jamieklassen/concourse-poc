module Msgs exposing (Msg(..))

import Http
import Models


type Msg
    = OnFetchData (Result Http.Error (List Models.Team))
