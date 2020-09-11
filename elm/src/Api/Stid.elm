module Api.Stid exposing (clientId, pids, systems)

import Dict
import Document
import Http
import Json.Decode as D
import Json.Encode as E
import Messages exposing (..)
import Plant
import System
import Url
import Url.Builder exposing (QueryParameter, int, string)


type Environment
    = Test
    | Production


environment : Environment
environment =
    Test


clientId : String
clientId =
    case environment of
        Test ->
            "b827c278-12de-47a0-b789-c8d11e3b9571/.default"

        Production ->
            "1734406c-3449-4192-a50d-7c3a63d3f57d/.default"


baseUrl : String
baseUrl =
    case environment of
        Test ->
            "https://stidapitest.equinor.com"

        Production ->
            "https://stidapi.equinor.com"


url : List String -> List QueryParameter -> String
url paths queryParams =
    Url.Builder.crossOrigin baseUrl paths queryParams


pids : Plant.StidPlant -> String -> Cmd Msg
pids plant token =
    Http.request
        { method = "GET"
        , url =
            url
                [ plant.name
                , "documents"
                ]
                [ string "docGroup" "P&ID"
                , int "take" 500000
                ]
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect = Http.expectJson (GotPidsFromStid plant) (D.list Document.decoder)
        , timeout = Nothing
        , tracker = Nothing
        }


systems : Plant.StidPlant -> String -> Cmd Msg
systems plant token =
    Http.request
        { method = "GET"
        , url =
            url
                [ plant.name
                , "system"
                ]
                [ string "isValid" "true" ]
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ token) ]
        , body = Http.emptyBody
        , expect = Http.expectJson (GotSystemsFromStid plant) (D.list System.decoder)
        , timeout = Nothing
        , tracker = Nothing
        }
