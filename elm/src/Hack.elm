module Hack exposing (..)

import Element exposing (htmlAttribute)
import Html.Attributes as HA


scrollbar =
    htmlAttribute <| HA.style "min-height" "auto"
