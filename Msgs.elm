module Msgs exposing (..)

import RemoteData as RD
import Models


type Msg
    = OnFetchPipelines (RD.WebData (List Models.Pipeline))
