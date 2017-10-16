module Kintail.Script.Platform
    exposing
        ( Platform(..)
        , lineSeparator
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
            "\x0D\n"
