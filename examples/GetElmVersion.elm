port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script, ProcessError(..))


script : List String -> Script Int ()
script arguments =
    Script.execute "elm" [ "--version" ]
        |> Script.map String.trim
        |> Script.andThen
            (\versionString ->
                Script.print ("Current Elm version: " ++ versionString)
            )
        |> Script.onError (toErrorString >> handleError)


toErrorString : ProcessError -> String
toErrorString processError =
    case processError of
        ProcessFailed message ->
            message

        ProcessWasTerminated ->
            "Process was terminated"

        ProcessExitedWithError code ->
            ("Process exited with code " ++ toString code)


handleError : String -> Script Int ()
handleError message =
    Script.print ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.program script requestPort responsePort
