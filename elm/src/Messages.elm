module Messages exposing (..)

import Document exposing (Document)
import Http
import Plant
import System exposing (System)
import Types exposing (TokenSuccess)


type Msg
    = DecodeError String
    | GotToken TokenSuccess
    | GotStidPlants (Result Http.Error (List String))
    | StidPlantsFromStorage (Maybe (List Plant.StidPlant))
    | StidPlantWasToggled Plant.StidPlant Bool
    | DocsFromStidButtonPressed
    | GotPidsFromStid Plant.StidPlant (Result Http.Error (List Document))
    | GotPidsFromStorage (Maybe (List Document))
    | GotSystemsFromStid Plant.StidPlant (Result Http.Error (List System))
    | GotSystemsFromStorage (Maybe (List System))
      --
    | GroupHeaderPressed String
