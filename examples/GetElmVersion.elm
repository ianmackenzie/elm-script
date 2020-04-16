module GetElmVersion exposing (main)

import Example
import Script exposing (Script)


script : Script.Init -> Script String ()
script { workingDirectory, userPrivileges } =
    Script.executeWith userPrivileges
        { command = "elm"
        , arguments = [ "--version" ]
        , workingDirectory = workingDirectory
        }
        |> Script.map String.trim
        |> Script.thenWith
            (\versionString ->
                Script.printLine ("Current Elm version: " ++ versionString)
            )


main : Script.Program
main =
    Example.program script
