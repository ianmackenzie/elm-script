port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)
import Kintail.Script.Process as Process exposing (Process)


script : Process -> Script Int ()
script process =
    Script.print "Hello World!"


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
