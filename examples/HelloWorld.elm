port module Main exposing (main)

import Json.Encode exposing (Value)
import Script exposing (Script)


script : Script.Context -> Script Int ()
script context =
    Script.printLine "Hello World!"


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
