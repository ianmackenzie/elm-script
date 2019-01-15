module Main exposing (main)

import Example
import Script exposing (Script)
import Script.EnvironmentVariables as EnvironmentVariables exposing (EnvironmentVariables)


printEnvironmentVariable : EnvironmentVariables -> String -> Script x ()
printEnvironmentVariable environmentVariables name =
    let
        value =
            EnvironmentVariables.get name environmentVariables
                |> Maybe.withDefault "not defined"
    in
    Script.printLine (name ++ ": " ++ value)


script : Script.Context -> Script Int ()
script { arguments, environmentVariables } =
    arguments |> Script.forEach (printEnvironmentVariable environmentVariables)


main : Script.Program
main =
    Example.program script
