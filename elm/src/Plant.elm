module Plant exposing (..)

import Json.Decode as D
import Json.Encode as E


type alias StidPlant =
    { name : String
    , active : Bool
    }


encoder : StidPlant -> E.Value
encoder plant =
    E.object
        [ ( "name", E.string plant.name )
        , ( "active", E.bool plant.active )
        ]


decoder : D.Decoder StidPlant
decoder =
    D.map2 StidPlant
        (D.field "name" D.string)
        (D.field "active" D.bool)
