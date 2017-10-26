port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.Shell as Shell exposing (ProcessError(..))


script : Script.Context -> Script Int ()
script { shell } =
    Shell.execute "elm --version" shell
        |> Script.map String.trim
        |> Script.andThen
            (\versionString ->
                Script.printLine ("Current Elm version: " ++ versionString)
            )
        |> Script.onError (handleError toErrorString)


toErrorString : ProcessError -> String
toErrorString processError =
    case processError of
        ProcessFailed message ->
            message

        ProcessWasTerminated ->
            "Process was terminated"

        ProcessExitedWithError code ->
            "Process exited with code " ++ toString code


handleError : (x -> String) -> x -> Script Int a
handleError toMessage error =
    Script.printLine ("ERROR: " ++ toMessage error)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
