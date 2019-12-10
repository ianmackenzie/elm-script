module Script.Path exposing (Path, Stat(..), encode, name, stat)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Regex exposing (Regex)
import Script.Internal as Internal


type alias Path =
    Internal.Path


type alias Error =
    { message : String
    }


type Stat
    = Nonexistent
    | File
    | Directory
    | Other


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


statDecoder : Decoder Stat
statDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "nonexistent" ->
                        Decode.succeed Nonexistent

                    "file" ->
                        Decode.succeed File

                    "directory" ->
                        Decode.succeed Directory

                    "other" ->
                        Decode.succeed Other

                    _ ->
                        Decode.fail ("Unrecognized stat value '" ++ string ++ "'")
            )


errorDecoder : Decoder Error
errorDecoder =
    Decode.map Error (Decode.field "message" Decode.string)


stat : Path -> Internal.Script Error Stat
stat path =
    Internal.Invoke "stat"
        (encode path)
        (Decode.oneOf
            [ statDecoder |> Decode.map Internal.Succeed
            , errorDecoder |> Decode.map Internal.Fail
            ]
        )
