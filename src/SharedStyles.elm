module SharedStyles exposing (..)

import Html.CssHelpers exposing (withNamespace)


type CssIds
    = Page


type CssClasses
    = Central
    | GameWidth


columnWidth =
    30


rascalNamespace =
    withNamespace "rascal"
