module Script.Environment exposing (Environment, get)

import Dict exposing (Dict)
import Script.Internal as Internal
import Script.Platform as Platform exposing (Platform(..))


type alias Environment =
    Internal.Environment


get : String -> Environment -> Maybe String
get name (Internal.Environment platform dict) =
    case platform of
        Windows ->
            Dict.get (String.toUpper name) dict

        Posix _ ->
            Dict.get name dict
