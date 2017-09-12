port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (FileError, Script)


printEnvironmentVariable : String -> Script x ()
printEnvironmentVariable name =
    Script.getEnvironmentVariable name
        |> Script.map (Maybe.withDefault "not defined")
        |> Script.andThen (\value -> Script.print (name ++ ": " ++ value))


script : List String -> Script Int ()
script names =
    names |> Script.forEach printEnvironmentVariable


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.program script requestPort responsePort
