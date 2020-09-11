module Tree exposing (..)

import Dict exposing (Dict)
import Document exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Equinor.Icon as Icon
import Equinor.Palette as Palette exposing (onClick)
import Messages exposing (..)
import Model exposing (Model)
import System exposing (System)


type GroupBy
    = System


emptyString =
    "00"


view : Model -> Element Msg
view model =
    column [ width fill, height fill, scrollbars ]
        [ model.documents
            |> Dict.values
            |> groupBySystem
            |> List.map (renderGroup model.stidSystems model.tree)
            |> column [ height fill, width fill, scrollbarY ]
        ]


renderGroup : Dict String System -> List String -> ( String, List Document ) -> Element Msg
renderGroup stidSystems activeItems ( groupName, items ) =
    let
        name =
            if String.isEmpty groupName then
                emptyString

            else
                groupName

        description =
            Dict.get groupName stidSystems
                |> Maybe.map .description
                |> Maybe.withDefault "No System!"

        isActive =
            List.member groupName activeItems
    in
    column [ width fill ]
        [ groupHeader name description isActive
        , el [ paddingEach { left = 24, top = 0, right = 0, bottom = 0 }, width fill ] <|
            if isActive then
                groupItems items

            else
                none
        ]


groupHeader : String -> String -> Bool -> Element Msg
groupHeader name description isActive =
    row
        [ width fill
        , padding 2
        , spacing 4
        , pointer
        , onClick (GroupHeaderPressed name)
        , mouseOver [ Background.color Palette.mistBlue ]
        , Font.size 12
        ]
        [ row []
            [ icon isActive
            , el
                [ width (px 14)
                , height (px 14)
                ]
                (html <|
                    if isActive then
                        Icon.folder_open

                    else
                        Icon.folder
                )
            ]
        , el [ width (px 16), Font.center ] (text (name ++ ","))
        , el [ Font.size 11 ] (text description)
        ]


groupItems : List Document -> Element Msg
groupItems documents =
    documents
        |> List.map renderDocument
        |> column [ width fill ]


renderDocument : Document -> Element Msg
renderDocument doc =
    row
        [ spacing 4
        , padding 2
        , pointer
        , mouseOver [ Background.color Palette.mistBlue ]
        ]
        [ el [ width (px 12), Font.color (rgb255 244 15 2) ] (html <| Icon.document True)
        , el [ Font.size 11 ] (text doc.docNo)
        ]


groupBySystem : List Document -> List ( String, List Document )
groupBySystem docs =
    groupBySystem_ docs Dict.empty
        |> Dict.toList


groupBySystem_ : List Document -> Dict String (List Document) -> Dict String (List Document)
groupBySystem_ documents dict =
    case documents of
        [] ->
            dict

        first :: rest ->
            let
                systemNo =
                    if String.isEmpty first.system then
                        emptyString

                    else
                        first.system

                updater mV =
                    mV
                        |> Maybe.map (\list -> first :: list)
                        |> Maybe.withDefault [ first ]
                        |> Just
            in
            groupBySystem_ rest (Dict.update systemNo updater dict)


icon : Bool -> Element Msg
icon isActive =
    let
        i =
            if isActive then
                Icon.arrow_down

            else
                Icon.arrow_right
    in
    el
        [ width (px 12)
        , height (px 12)
        , Font.color Palette.grey
        ]
        (html i)
