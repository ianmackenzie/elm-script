port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Allowed, FileError, Script)
import Kintail.Script.Process as Process exposing (Process)


printEnvironmentVariable : String -> Script { p | read : Allowed } x ()
printEnvironmentVariable name =
    Script.getEnvironmentVariable name
        |> Script.map (Maybe.withDefault "not defined")
        |> Script.andThen (\value -> Script.print (name ++ ": " ++ value))


script : List String -> Script { read : Allowed } Int ()
script names =
    names |> Script.forEach printEnvironmentVariable


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
