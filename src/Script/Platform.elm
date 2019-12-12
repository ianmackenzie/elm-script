module Script.Platform exposing
    ( Platform(..)
    , lineSeparator
    , pathSeparator
    )

import Json.Decode as Decode exposing (Decoder)


type Platform
    = Posix String
    | Windows


lineSeparator : Platform -> String
lineSeparator platform =
    case platform of
        Posix _ ->
            "\n"

        Windows ->
            "\u{000D}\n"


pathSeparator : Platform -> String
pathSeparator platform =
    case platform of
        Posix _ ->
            "/"

        Windows ->
            "\\"
