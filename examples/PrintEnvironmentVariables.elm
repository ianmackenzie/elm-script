module PrintEnvironmentVariables exposing (main)

import Script exposing (Script)
import Script.Environment as Environment exposing (Environment)


printEnvironmentVariable : Environment -> String -> Script x ()
printEnvironmentVariable environment name =
    let
        value =
            Environment.get name environment
                |> Maybe.withDefault "not defined"
    in
    Script.printLine (name ++ ": " ++ value)


script : Script.Init -> Script String ()
script { arguments, environment } =
    arguments |> Script.each (printEnvironmentVariable environment)


main : Script.Program
main =
    Script.program script
