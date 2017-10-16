port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Context, Script)
import Kintail.Script.Shell as Shell exposing (ProcessError(..))


script : Context -> Script Int ()
script { shell } =
    Shell.execute "elm --version" shell
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
            "Process exited with code " ++ toString code


handleError : String -> Script Int ()
handleError message =
    Script.do [ Script.print ("ERROR: " ++ message), Script.fail 1 ]


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
