module Script.Path exposing
    ( Path(..)
    , absolute
    , append
    , encode
    , join
    , name
    , resolve
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Regex exposing (Regex)
import Script.Platform as Platform exposing (Platform(..))


type Path
    = Path Platform (List String)


nameRegex : Regex
nameRegex =
    Regex.fromString "([^\\\\/]+)[\\\\/]*$"
        |> Maybe.withDefault Regex.never


name : Path -> String
name (Path platform components) =
    case Regex.find nameRegex (String.join "/" components) of
        [ { match, submatches } ] ->
            case submatches of
                [ Just name_ ] ->
                    name_

                _ ->
                    ""

        _ ->
            ""


encode : Path -> Value
encode (Path platform components) =
    Encode.list Encode.string components


append : String -> Path -> Path
append component (Path platform components) =
    Path platform (components ++ [ component ])


absolute : Platform -> String -> Path
absolute platform string =
    Path platform [ string ]


resolve : Path -> String -> Path
resolve (Path platform parentComponents) pathString =
    if isAbsolute platform pathString then
        Path platform [ pathString ]

    else
        Path platform [ join platform (parentComponents ++ [ pathString ]) ]


isAbsolute : Platform -> String -> Bool
isAbsolute platform pathString =
    case platform of
        Posix _ ->
            String.startsWith "/" pathString

        Windows ->
            String.startsWith "\\\\" pathString || startsAtWindowsDriveRoot pathString


startsAtWindowsDriveRoot : String -> Bool
startsAtWindowsDriveRoot pathString =
    case String.uncons pathString of
        Just ( firstCharacter, rest ) ->
            Char.isAlpha firstCharacter
                && (String.startsWith ":/" rest || String.startsWith ":\\" rest)

        Nothing ->
            False


join : Platform -> List String -> String
join platform components =
    case components of
        first :: rest ->
            let
                joinedComponents =
                    String.join (Platform.pathSeparator platform)
                        (List.concatMap pathChunks components)
            in
            if isAbsolute platform first then
                case platform of
                    Posix _ ->
                        "/" ++ joinedComponents

                    Windows ->
                        if String.startsWith "\\\\" first then
                            "\\\\" ++ joinedComponents

                        else
                            joinedComponents

            else
                joinedComponents

        [] ->
            ""


pathChunks : String -> List String
pathChunks string =
    Regex.find pathChunk string |> List.map .match


pathChunk : Regex
pathChunk =
    Regex.fromString "[^\\\\/]+" |> Maybe.withDefault Regex.never


toString : Path -> String
toString (Path platform components) =
    join platform components
