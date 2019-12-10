module PowerShellTest exposing (..)

import Example
import Script exposing (Script)
import Script.Shell as Shell


script : Script.Context -> Script Int ()
script { shell } =
    Shell.execute "PowerShell -Command Get-ChildItem -Name -Path *.elm" shell
        |> Script.map String.lines
        |> Script.map (List.map String.trim)
        |> Script.map (List.filter (not << String.isEmpty))
        |> Script.andThen (Script.forEach (String.toUpper >> Script.printLine))
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
