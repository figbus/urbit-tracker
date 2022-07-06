module Indigo exposing (..)

import Element exposing (Attribute, Color, Element, rgb255, spacing)
import Element.Font as Font
import Svg exposing (svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, stroke, strokeLinecap, strokeWidth, viewBox)


interUi : Attribute msg
interUi =
    Font.family
        [ Font.external
            { url = "https://rsms.me/inter/inter.css"
            , name = "Inter"
            }
        , Font.sansSerif
        ]


header1 : List (Attribute msg)
header1 =
    [ interUi
    , Font.extraBold
    , Font.size 32
    , Font.letterSpacing -1
    ]


header1Spacing : Attribute msg
header1Spacing =
    spacing 16


paragraphA : List (Attribute msg)
paragraphA =
    [ interUi
    , Font.regular
    , Font.size 14
    ]


paragraphASpacing : Attribute msg
paragraphASpacing =
    spacing 10


buttons : List (Attribute msg)
buttons =
    [ interUi
    , Font.semiBold
    , Font.size 14
    ]


buttonsSpacing : Attribute msg
buttonsSpacing =
    spacing 2


normalLabel : List (Attribute msg)
normalLabel =
    [ interUi
    , Font.regular
    , Font.size 14
    ]


normalLabelSpacing : Attribute msg
normalLabelSpacing =
    spacing 2


smallLabelBold : List (Attribute msg)
smallLabelBold =
    [ interUi
    , Font.semiBold
    , Font.size 12
    ]


smallLabelBoldSpacing : Attribute msg
smallLabelBoldSpacing =
    spacing 4



-- COLORS


{-| Primary Background
-}
white : Color
white =
    rgb255 255 255 255


{-| Secondary Background
-}
black04 : Color
black04 =
    rgb255 245 245 245


black10 : Color
black10 =
    rgb255 229 229 229


black20 : Color
black20 =
    rgb255 204 204 204


{-| Tertiary Text
-}
black40 : Color
black40 =
    rgb255 153 153 153


{-| Secondary Text
-}
black60 : Color
black60 =
    rgb255 102 102 102


{-| Primary Text
-}
black80 : Color
black80 =
    rgb255 51 51 51


black100 : Color
black100 =
    rgb255 0 0 0


red : Color
red =
    rgb255 255 98 64


blue : Color
blue =
    rgb255 0 142 255


softRed : Color
softRed =
    rgb255 253 238 235



-- ICONOGRAPHY


smallCross : Element msg
smallCross =
    svg
        [ Svg.Attributes.width "16"
        , Svg.Attributes.height "16"
        , viewBox "0 0 16 16"
        , Svg.Attributes.fill "none"
        ]
        [ Svg.path
            [ d "M8 8L12 4M8 8L4 4M8 8L4 12M8 8L12 12"
            , stroke "currentColor"
            , strokeWidth "2"
            , strokeLinecap "round"
            ]
            []
        ]
        |> Element.html
        |> Element.el []


smallCheck : Element msg
smallCheck =
    svg
        [ Svg.Attributes.width "16"
        , Svg.Attributes.height "16"
        , viewBox "0 0 16 16"
        , Svg.Attributes.fill "none"
        ]
        [ Svg.path
            [ d "M5 8L6.64645 9.64645C6.84171 9.84171 7.15829 9.84171 7.35355 9.64645L11 6"
            , stroke "currentColor"
            , strokeWidth "2"
            , strokeLinecap "round"
            ]
            []
        ]
        |> Element.html
        |> Element.el []


smallPlus : Element msg
smallPlus =
    svg
        [ Svg.Attributes.width "16"
        , Svg.Attributes.height "16"
        , viewBox "0 0 16 16"
        , Svg.Attributes.fill "none"
        ]
        [ Svg.path
            [ d "M3 8H13M8 3V13"
            , stroke "currentColor"
            , strokeWidth "2"
            , strokeLinecap "round"
            ]
            []
        ]
        |> Element.html
        |> Element.el []


check : Element msg
check =
    svg
        [ Svg.Attributes.width "24"
        , Svg.Attributes.height "24"
        , viewBox "0 0 24 24"
        , Svg.Attributes.fill "none"
        ]
        [ Svg.path
            [ d "M6 12L9.64645 15.6464C9.84171 15.8417 10.1583 15.8417 10.3536 15.6464L18 8"
            , stroke "currentColor"
            , strokeWidth "2"
            , strokeLinecap "round"
            ]
            []
        ]
        |> Element.html
        |> Element.el []


plus : Element msg
plus =
    svg
        [ Svg.Attributes.width "24"
        , Svg.Attributes.height "24"
        , viewBox "0 0 24 24"
        , Svg.Attributes.fill "none"
        ]
        [ Svg.path
            [ d "M4 12H20M12 4V20"
            , stroke "currentColor"
            , strokeWidth "2"
            , strokeLinecap "round"
            ]
            []
        ]
        |> Element.html
        |> Element.el []


optionsA : Element msg
optionsA =
    svg
        [ Svg.Attributes.width "24"
        , Svg.Attributes.height "24"
        , viewBox "0 0 24 24"
        , Svg.Attributes.fill "none"
        ]
        [ Svg.path
            [ fillRule "evenodd"
            , clipRule "evenodd"
            , d "M12 21C7.02944 21 3 16.9706 3 12C3 7.02944 7.02944 3 12 3C16.9706 3 21 7.02944 21 12C21 16.9706 16.9706 21 12 21ZM9 12C9 12.5523 8.55228 13 8 13C7.44772 13 7 12.5523 7 12C7 11.4477 7.44772 11 8 11C8.55228 11 9 11.4477 9 12ZM12 13C12.5523 13 13 12.5523 13 12C13 11.4477 12.5523 11 12 11C11.4477 11 11 11.4477 11 12C11 12.5523 11.4477 13 12 13ZM16 13C16.5523 13 17 12.5523 17 12C17 11.4477 16.5523 11 16 11C15.4477 11 15 11.4477 15 12C15 12.5523 15.4477 13 16 13Z"
            , fill "currentColor"
            ]
            []
        ]
        |> Element.html
        |> Element.el []
