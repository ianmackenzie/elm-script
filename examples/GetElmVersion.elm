module Main exposing (..)

import Example
import Script exposing (Script)
import Script.Shell as Shell


script : Script.Context -> Script Int ()
script { shell, workingDirectory } =
    shell
        |> Shell.executeWith [ Shell.workingDirectory workingDirectory ]
            "elm --version"
        |> Script.map String.trim
        |> Script.andThen
            (\versionString ->
                Script.printLine ("Current Elm version: " ++ versionString)
            )
        |> Script.onError (Example.handleError toErrorString)


toErrorString : Shell.ProcessError -> String
toErrorString processError =
    case processError of
        Shell.ProcessFailed message ->
            message

        Shell.ProcessWasTerminated ->
            "Process was terminated"

        Shell.ProcessExitedWithError code ->
            "Process exited with code " ++ String.fromInt code


main : Script.Program
main =
    Example.program script
