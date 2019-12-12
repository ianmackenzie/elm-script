module Script.Environment exposing (Environment, get)

import Dict exposing (Dict)
import Script.Internal as Internal
import Script.PlatformType as PlatformType exposing (PlatformType(..))


type alias Environment =
    Internal.Environment


get : String -> Environment -> Maybe String
get name (Internal.Environment platform dict) =
    case platform of
        Windows ->
            Dict.get (String.toUpper name) dict

        Posix ->
            Dict.get name dict
