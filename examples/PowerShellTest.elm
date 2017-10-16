port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Context, Script)
import Script.Shell as Shell exposing (ProcessError(..))


script : Context -> Script Int ()
script { shell } =
    Shell.execute "PowerShell -Command Get-ChildItem -Name -Path *.elm" shell
        |> Script.map String.lines
        |> Script.map (List.map String.trim)
        |> Script.map (List.filter (not << String.isEmpty))
        |> Script.andThen (Script.forEach (String.toUpper >> Script.print))
        |> Script.onError (toErrorString >> handleError)


toErrorString : ProcessError -> String
toErrorString processError =
    case processError of
        ProcessFailed message ->
            message

        ProcessWasTerminated ->
            "Process was terminated"

        ProcessExitedWithError code ->
            "Process exited with code " ++ toString code


handleError : String -> Script Int a
handleError message =
    Script.print ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
