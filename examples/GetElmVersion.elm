module GetElmVersion exposing (main)

import Example
import Script exposing (Script)


script : List String -> Script.WorkingDirectory -> Script.Host -> Script Int ()
script arguments workingDirectory host =
    host.execute "elm" [ "--version" ]
        |> Script.map String.trim
        |> Script.andThen
            (\versionString ->
                Script.printLine ("Current Elm version: " ++ versionString)
            )
        |> Script.onError (Example.handleError toErrorString)


toErrorString : Script.SubprocessError -> String
toErrorString processError =
    case processError of
        Script.ExecutableNotFound ->
            "Process executable not found"

        Script.SubprocessFailed message ->
            message

        Script.SubprocessWasTerminated ->
            "Process was terminated"

        Script.SubprocessExitedWithError code ->
            "Process exited with code " ++ String.fromInt code


main : Script.Program
main =
    Example.program script
