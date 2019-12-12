module PrintEnvironmentVariables exposing (main)

import Example
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


script :
    List String
    -> Script.WorkingDirectory
    -> Script.Host
    -> Script.UserPrivileges
    -> Script Int ()
script arguments workingDirectory host userPrivileges =
    arguments |> Script.forEach (printEnvironmentVariable host.environment)


main : Script.Program
main =
    Example.program script
