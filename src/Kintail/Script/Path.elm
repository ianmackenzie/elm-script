module Kintail.Script.Path exposing (Path, name, separator)

import Kintail.Script.Internal as Internal
import Kintail.Script.Platform as Platform exposing (Platform)
import Regex exposing (Regex)


type alias Path =
    Internal.Path


nameRegex : Regex
nameRegex =
    Regex.regex "([^\\\\/]+)[\\\\/]*$"


name : Path -> String
name path =
    case Regex.find (Regex.AtMost 1) nameRegex (String.join "/" path) of
        [ { match, submatches } ] ->
            case submatches of
                [ Just name ] ->
                    name

                _ ->
                    ""

        _ ->
            ""


separator : Platform -> String
separator platform =
    case platform of
        Platform.Posix ->
            "/"

        Platform.Windows ->
            "\\"
