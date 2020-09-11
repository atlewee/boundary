module Document exposing (..)

import Browser exposing (Document)
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E


type alias Document =
    { docNo : String
    , docTitle : String
    , system : String
    , revStatus : String
    , instCode : String
    }


decoder : D.Decoder Document
decoder =
    D.succeed Document
        |> required "docNo" D.string
        |> required "docTitle" D.string
        |> required "system" nullString
        |> required "revStatus" D.string
        |> required "instCode" D.string


encoder : Document -> E.Value
encoder doc =
    E.object
        [ ( "docNo", E.string doc.docNo )
        , ( "docTitle", E.string doc.docTitle )
        , ( "system", E.string doc.system )
        , ( "revStatus", E.string doc.revStatus )
        , ( "instCode", E.string doc.instCode )
        ]


nullString =
    D.oneOf [ D.string, D.null "" ]
