port module Main exposing (..)

import Kintail.Script as Script exposing (Script)
import Json.Encode exposing (Value)
import Time


script : List String -> Script Int ()
script _ =
    Script.perform Time.now
        |> Script.andThen
            (\time ->
                if (truncate time % 100 > 87) then
                    Script.print "Succeeded"
                        |> Script.andThen (\() -> Script.succeed ())
                else
                    Script.print "Failed"
                        |> Script.andThen (\() -> Script.fail 1)
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.program script requestPort responsePort
