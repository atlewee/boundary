module Model exposing (..)

import Dict exposing (Dict)
import Document exposing (Document)
import Messages exposing (..)
import Plant
import System exposing (System)


type alias Model =
    { pcsPlantId : String
    , stidPlants : List Plant.StidPlant
    , stidSystems : Dict String System
    , requests : Dict Int (List (String -> Cmd Msg))
    , documents : Dict String Document
    , tree : List String
    }


type alias Flags =
    { plantId : String
    }
