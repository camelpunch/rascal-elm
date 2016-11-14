module RascalStyles exposing (css)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)
import SharedStyles exposing (..)


css =
    (stylesheet << namespace rascalNamespace.name)
        [ body
            [ backgroundColor (rgb 0 0 0)
            , fontFamilies [ "Courier New" ]
            ]
        , (#) Page
            [ margin2 zero auto
            , color (rgb 255 255 255)
            ]
        , (.) GameWidth
            [ width (em columnWidth) ]
        , (.) Central
            [ textAlign center ]
        ]
