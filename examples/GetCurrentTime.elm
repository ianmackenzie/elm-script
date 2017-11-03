port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Time


script : Script.Context -> Script Int ()
script context =
    Script.getCurrentTime
        |> Script.andThen
            (\currentTime ->
                Script.printLine <|
                    "Number of hours since January 1, 1970: "
                        ++ toString (Time.inHours currentTime)
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
