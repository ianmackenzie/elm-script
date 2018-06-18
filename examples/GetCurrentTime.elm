port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Time


script : Script.Context -> Script Int ()
script context =
    Script.getCurrentTime
        |> Script.andThen
            (\currentTime ->
                let
                    millisecondsSinceEpoch =
                        toFloat (Time.posixToMillis currentTime)

                    hoursSinceEpoch =
                        millisecondsSinceEpoch / (1000 * 60 * 60)
                in
                Script.printLine <|
                    "Number of hours since January 1, 1970: "
                        ++ String.fromFloat hoursSinceEpoch
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
