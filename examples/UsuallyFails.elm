port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Time


script : Script.Context -> Script Int ()
script context =
    Script.getCurrentTime
        |> Script.andThen
            (\time ->
                if (Time.posixToMillis time |> modBy 100) > 87 then
                    Script.printLine "Succeeded"
                else
                    Script.printLine "Failed"
                        |> Script.andThen (\() -> Script.fail 1)
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
