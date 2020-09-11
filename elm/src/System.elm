module System exposing (..)

import Json.Decode as D
import Json.Encode as E


type alias System =
    { system : String
    , description : String
    }


encoder : System -> E.Value
encoder system =
    E.object
        [ ( "system", E.string system.system )
        , ( "description", E.string system.description )
        ]


decoder : D.Decoder System
decoder =
    D.map2 System
        (D.field "system" D.string)
        (D.field "description" D.string)
