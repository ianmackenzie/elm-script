module Script.Platform exposing
    ( Platform(..)
    , lineSeparator
    , pathSeparator
    )


type Platform
    = Posix
    | Windows


lineSeparator : Platform -> String
lineSeparator platform =
    case platform of
        Posix ->
            "\n"

        Windows ->
            "\u{000D}\n"


pathSeparator : Platform -> String
pathSeparator platform =
    case platform of
        Posix ->
            "/"

        Windows ->
            "\\"
