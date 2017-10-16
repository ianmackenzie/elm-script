port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Context, Script)
import Kintail.Script.EnvironmentVariables as EnvironmentVariables exposing (EnvironmentVariables)


printEnvironmentVariable : EnvironmentVariables -> String -> Script x ()
printEnvironmentVariable environmentVariables name =
    let
        value =
            EnvironmentVariables.get name environmentVariables
                |> Maybe.withDefault "not defined"
    in
    Script.print (name ++ ": " ++ value)


script : Context -> Script Int ()
script { arguments, environmentVariables } =
    arguments |> Script.forEach (printEnvironmentVariable environmentVariables)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
