port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)
import Time


script : List String -> Script Int ()
script _ =
    Script.perform Time.now
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