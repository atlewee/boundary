module Api.CommonLib exposing (clientId, stidPlantCode)

import Http
import Json.Decode as D
import Url
import Url.Builder exposing (QueryParameter, string)


clientId : String
clientId =
    "37d598fc-da0f-46bd-949f-7107918d47a5/.default"


url : List String -> List QueryParameter -> String
url paths queryParams =
    Url.Builder.crossOrigin
        "https://infield.equinor.com"
        ("commonLibProxy" :: "api" :: paths)
        queryParams


type alias Attribute =
    { definitionName : String
    , displayValue : String
    }


attributeDecoder : D.Decoder Attribute
attributeDecoder =
    D.map2 Attribute
        (D.field "definitionName" D.string)
        (D.field "displayValue" D.string)


type alias Facility =
    { name : String
    , attributes : List Attribute
    }


facilityDecoder : D.Decoder Facility
facilityDecoder =
    D.map2 Facility
        (D.field "name" D.string)
        (D.field "attributes" (D.list attributeDecoder))


stidPlantsDecoder : String -> D.Decoder (List String)
stidPlantsDecoder pcsPlantId =
    D.list facilityDecoder
        |> D.andThen
            (\facilities ->
                facilities
                    |> List.filterMap (hasThisProcosysId pcsPlantId)
                    |> D.succeed
            )


hasThisProcosysId : String -> Facility -> Maybe String
hasThisProcosysId pcsPlantId facility =
    facility.attributes
        |> List.filterMap
            (\att ->
                if att.definitionName == "ProjectSchema" && att.displayValue == pcsPlantId then
                    Just facility.name

                else
                    Nothing
            )
        |> List.head


stidPlantCode pcsPlantId msg token =
    Http.request
        { method = "GET"
        , url =
            url [ "Code", "ApplicationProCoSys" ]
                []
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect = Http.expectJson msg (stidPlantsDecoder pcsPlantId)
        , timeout = Nothing
        , tracker = Nothing
        }
