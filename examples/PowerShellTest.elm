module PowerShellTest exposing (main)

import Example
import Script exposing (Script)


script : Script.Init -> Script Int ()
script { workingDirectory, userPrivileges } =
    Script.executeWith userPrivileges
        { command = "PowerShell"
        , arguments = [ "-Command", "Get-ChildItem", "-Name", "-Path", "*.elm" ]
        , workingDirectory = workingDirectory
        }
        |> Script.map String.lines
        |> Script.map (List.map String.trim)
        |> Script.map (List.filter (not << String.isEmpty))
        |> Script.andThen (Script.forEach (String.toUpper >> Script.printLine))
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
