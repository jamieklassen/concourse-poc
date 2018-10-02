module Msgs exposing (Msg(..))

import Http
import Models
import Monocle.Optional as MO
import Time exposing (Time)
import Window


type Msg
    = OnFetchData (Result Http.Error Models.Data)
    | ToggleHighDensity
    | TogglePipelinePaused (MO.Optional Models.Model Models.Pipeline)
    | Tick Time
    | SetScreenSize Window.Size
