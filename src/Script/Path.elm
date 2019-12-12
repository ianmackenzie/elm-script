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
import Script.PlatformType as PlatformType exposing (PlatformType(..))


type Path
    = Path PlatformType (List String)


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
append component (Path platformType components) =
    Path platformType (components ++ [ component ])


absolute : PlatformType -> String -> Path
absolute platformType string =
    Path platformType [ string ]


resolve : Path -> String -> Path
resolve (Path platformType parentComponents) pathString =
    if isAbsolute platformType pathString then
        Path platformType [ pathString ]

    else
        Path platformType [ join platformType (parentComponents ++ [ pathString ]) ]


isAbsolute : PlatformType -> String -> Bool
isAbsolute platformType pathString =
    case platformType of
        Posix ->
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


join : PlatformType -> List String -> String
join platformType components =
    case components of
        first :: rest ->
            let
                separator =
                    case platformType of
                        Posix ->
                            "/"

                        Windows ->
                            "\\"

                joinedComponents =
                    String.join separator (List.concatMap pathChunks components)
            in
            if isAbsolute platformType first then
                case platformType of
                    Posix ->
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
toString (Path platformType components) =
    join platformType components
