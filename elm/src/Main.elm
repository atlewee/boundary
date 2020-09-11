port module Main exposing (main)

import Api.CommonLib
import Api.Stid
import Browser
import Dict exposing (Dict)
import Document exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Equinor.Palette as Palette exposing (onClick)
import Hack
import Html as H
import Html.Attributes as HA
import Http
import Json.Decode as D
import Json.Encode as E
import Messages exposing (..)
import Model exposing (..)
import Plant
import System exposing (System)
import Tree
import Types exposing (..)


port toJs : E.Value -> Cmd msg


port fromJs : (E.Value -> msg) -> Sub msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = initialModel
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


initialModel : Flags -> ( Model, Cmd Msg )
initialModel flags =
    ( { pcsPlantId = flags.plantId
      , stidPlants = []
      , stidSystems = Dict.empty
      , requests = Dict.empty
      , documents = Dict.empty
      , tree = []
      }
    , Cmd.batch [ getFromStorage "stidPlants" ]
    )


getFromStorage key =
    jsMsg "get" (E.string key)


putInStorage key value =
    jsMsg "put"
        (E.object
            [ ( "key", E.string key )
            , ( "payload", value )
            ]
        )


putStidPlantsInStorage : List Plant.StidPlant -> Cmd Msg
putStidPlantsInStorage stidPlants =
    putInStorage "stidPlants" (E.list Plant.encoder stidPlants)


putDocsInStorage : Dict String Document -> Cmd Msg
putDocsInStorage docDict =
    putInStorage "documents" <|
        E.list Document.encoder (Dict.values docDict)


putSystemsInStorage : Dict String System -> Cmd Msg
putSystemsInStorage systemDict =
    putInStorage "systems" <|
        E.list System.encoder (Dict.values systemDict)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        mc =
            ( model, Cmd.none )
    in
    case msg of
        DecodeError str ->
            let
                _ =
                    Debug.log "DecodeError" str
            in
            ( model, Cmd.none )

        GotToken tokenSuccess ->
            let
                maybeDoReq =
                    Dict.get tokenSuccess.refNo model.requests
            in
            ( { model
                | requests = Dict.remove tokenSuccess.refNo model.requests
              }
            , case maybeDoReq of
                Just doReqList ->
                    doReqList
                        |> List.map (\fn -> fn tokenSuccess.token)
                        |> Cmd.batch

                Nothing ->
                    Cmd.none
            )

        GotStidPlants result ->
            case result of
                Ok plants ->
                    let
                        stidPlants =
                            plants
                                |> List.map (\name -> Plant.StidPlant name False)
                    in
                    ( { model | stidPlants = stidPlants }
                    , putStidPlantsInStorage stidPlants
                    )

                Err err ->
                    ( model, Cmd.none )

        StidPlantsFromStorage maybePlants ->
            case maybePlants of
                Just plants ->
                    mc
                        |> updatePlantsInModel plants
                        |> getDataFromStorage [ "documents", "systems" ]

                Nothing ->
                    getPlantsFromStid mc

        StidPlantWasToggled plant nextState ->
            let
                nextPlants =
                    model.stidPlants
                        |> List.map
                            (\p ->
                                if p.name == plant.name then
                                    { p | active = nextState }

                                else
                                    p
                            )
            in
            ( { model | stidPlants = nextPlants }, putStidPlantsInStorage nextPlants )

        DocsFromStidButtonPressed ->
            ( model, Cmd.none )
                |> apiRequest Api.Stid.clientId
                    (model.stidPlants
                        |> List.filter .active
                        |> List.map (\plant -> Api.Stid.pids plant)
                    )

        GotPidsFromStid plant result ->
            case result of
                Ok documents ->
                    let
                        docDict =
                            List.foldl (\doc dict -> Dict.insert (doc.docNo ++ doc.revStatus) doc dict) model.documents documents
                    in
                    ( { model | documents = docDict }, putDocsInStorage docDict )

                Err err ->
                    ( model, Cmd.none )

        GotPidsFromStorage maybeDocuments ->
            case maybeDocuments of
                Just documents ->
                    ( { model | documents = List.foldl (\doc dict -> Dict.insert (doc.docNo ++ doc.revStatus) doc dict) Dict.empty documents }, Cmd.none )

                Nothing ->
                    mc

        GotSystemsFromStid plant result ->
            case result of
                Ok systems ->
                    let
                        systemDict =
                            List.foldl (\system dict -> Dict.insert system.system system dict) model.stidSystems systems
                    in
                    ( { model | stidSystems = systemDict }, putSystemsInStorage systemDict )

                Err err ->
                    ( model, Cmd.none )

        GotSystemsFromStorage maybeSystems ->
            case maybeSystems of
                Just systems ->
                    ( { model | stidSystems = List.foldl (\system dict -> Dict.insert system.system system dict) Dict.empty systems }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )
                        |> apiRequest Api.Stid.clientId
                            (model.stidPlants
                                |> List.filter .active
                                |> List.map (\plant -> Api.Stid.systems plant)
                            )

        GroupHeaderPressed name ->
            mc
                |> toggleTreeGroup name


subscriptions : Model -> Sub Msg
subscriptions model =
    fromJs handleJsMsg


toggleTreeGroup : String -> MC -> MC
toggleTreeGroup name ( m, c ) =
    let
        nextTree =
            if List.member name m.tree then
                List.filter (\x -> x /= name) m.tree

            else
                name :: m.tree
    in
    ( { m | tree = nextTree }, c )


getPlantsFromStid : MC -> MC
getPlantsFromStid ( m, c ) =
    ( m, c ) |> apiRequest Api.CommonLib.clientId [ Api.CommonLib.stidPlantCode m.pcsPlantId GotStidPlants ]


updatePlantsInModel : List Plant.StidPlant -> MC -> MC
updatePlantsInModel plants ( m, c ) =
    ( { m | stidPlants = plants }, c )


getDataFromStorage : List String -> MC -> MC
getDataFromStorage keys ( m, c ) =
    ( m
    , Cmd.batch
        [ c
        , Cmd.batch (List.map getFromStorage keys)
        ]
    )


handleJsMsg : D.Value -> Msg
handleJsMsg jsValue =
    case D.decodeValue jsMsgDecoder jsValue of
        Ok jsmsg ->
            jsmsg

        Err err ->
            DecodeError (Debug.toString err)


jsMsgDecoder : D.Decoder Msg
jsMsgDecoder =
    D.field "topic" D.string
        |> D.andThen
            (\topic ->
                D.field "payload" <|
                    case topic of
                        "got" ->
                            D.field "key" D.string
                                |> D.andThen
                                    (\key ->
                                        D.field "data"
                                            (case key of
                                                "stidPlants" ->
                                                    D.map
                                                        StidPlantsFromStorage
                                                        (D.oneOf
                                                            [ D.map Just (D.list Plant.decoder)
                                                            , D.null Nothing
                                                            ]
                                                        )

                                                "documents" ->
                                                    D.map GotPidsFromStorage
                                                        (D.oneOf
                                                            [ D.map Just (D.list Document.decoder)
                                                            , D.null Nothing
                                                            ]
                                                        )

                                                "systems" ->
                                                    D.map GotSystemsFromStorage
                                                        (D.oneOf
                                                            [ D.map Just (D.list System.decoder)
                                                            , D.succeed Nothing
                                                            ]
                                                        )

                                                _ ->
                                                    D.fail "Unknown key"
                                            )
                                    )

                        "gotToken" ->
                            D.map GotToken
                                (D.map2 TokenSuccess
                                    (D.field "refNo" D.int)
                                    (D.field "token" D.string)
                                )

                        _ ->
                            D.fail "Unknown msg from Js"
            )


view : Model -> H.Html Msg
view model =
    layout
        [ width fill
        , height fill
        , scrollbars
        , Font.family [ Font.typeface "equinor" ]
        ]
        (column [ width fill, height fill, scrollbars ]
            [ header model
            , el [ padding 10, height fill, Border.width 1, width (fill |> maximum 300) ] (Tree.view model)
            ]
        )


header : Model -> Element Msg
header model =
    wrappedRow [ spacing 10, width fill, padding 10, Background.color Palette.mistBlue ]
        [ el [] (text "Included STID plants:")
        , model.stidPlants
            |> List.map selectablePlant
            |> row [ spacing -1 ]
        , el [ alignRight, padding 4, pointer, onClick DocsFromStidButtonPressed, Border.rounded 4, Border.width 1 ] (text "Get docs from Stid")
        ]


selectablePlant plant =
    row [ Border.width 1, Border.color Palette.mistBlue, paddingXY 10 2 ]
        [ Input.checkbox [ moveUp 1 ]
            { onChange = StidPlantWasToggled plant
            , icon = Input.defaultCheckbox
            , label = Input.labelHidden ""
            , checked = plant.active
            }
        , el [] (text plant.name)
        ]


type alias MC =
    ( Model, Cmd Msg )


apiRequest : String -> List (String -> Cmd Msg) -> MC -> MC
apiRequest clientId requests ( m, c ) =
    let
        highestRefNo =
            m.requests
                |> Dict.keys
                |> List.maximum
                |> Maybe.withDefault 0

        nextRef =
            highestRefNo + 1
    in
    ( { m | requests = Dict.insert nextRef requests m.requests }
    , Cmd.batch
        [ c
        , jsMsg "getToken"
            (E.object
                [ ( "clientId", E.string clientId )
                , ( "refNo", E.int nextRef )
                ]
            )
        ]
    )


jsMsg : String -> E.Value -> Cmd Msg
jsMsg topic payload =
    E.object
        [ ( "topic", E.string topic )
        , ( "payload", payload )
        ]
        |> toJs
