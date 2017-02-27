port module Main exposing (..)

import Kintail.Script as Script
import Json.Encode exposing (Value)


script : List String -> Script Int ()
script arguments =
    Script.print "Hello World!"


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.program script requestPort responsePort
