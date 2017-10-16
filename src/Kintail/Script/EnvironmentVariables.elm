module Kintail.Script.EnvironmentVariables exposing (EnvironmentVariables, get)

import Dict
import Kintail.Script.Internal as Internal
import Kintail.Script.Platform as Platform exposing (Platform(..))


type alias EnvironmentVariables =
    Internal.EnvironmentVariables


get : String -> EnvironmentVariables -> Maybe String
get name (Internal.EnvironmentVariables platform dict) =
    case platform of
        Windows ->
            Dict.get (String.toUpper name) dict

        Posix ->
            Dict.get name dict
