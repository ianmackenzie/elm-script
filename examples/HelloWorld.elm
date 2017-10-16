port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Context, Script)


script : Context -> Script Int ()
script context =
    Script.print "Hello World!"


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
