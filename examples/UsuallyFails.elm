port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Context, Script)


script : Context -> Script Int ()
script context =
    Script.getCurrentTime
        |> Script.andThen
            (\time ->
                if truncate time % 100 > 87 then
                    Script.print "Succeeded"
                else
                    Script.do [ Script.print "Failed", Script.fail 1 ]
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
