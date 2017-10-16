module Script.Path exposing (Path, name, separator)

import Regex exposing (Regex)
import Script.Internal as Internal
import Script.Platform as Platform exposing (Platform)


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
