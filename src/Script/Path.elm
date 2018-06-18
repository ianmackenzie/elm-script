module Script.Path exposing (Path, encode, name)

import Json.Encode as Encode exposing (Value)
import Regex exposing (Regex)
import Script.Internal as Internal


type alias Path =
    Internal.Path


nameRegex : Regex
nameRegex =
    Regex.fromString "([^\\\\/]+)[\\\\/]*$"
        |> Maybe.withDefault Regex.never


name : Path -> String
name path =
    case Regex.find nameRegex (String.join "/" path) of
        [ { match, submatches } ] ->
            case submatches of
                [ Just name_ ] ->
                    name_

                _ ->
                    ""

        _ ->
            ""


encode : Path -> Value
encode path =
    Encode.list Encode.string path
