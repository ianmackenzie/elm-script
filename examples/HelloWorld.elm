port module HelloWorld exposing (main)

import Json.Encode exposing (Value)
import Script exposing (Script)


script : List String -> Script.WorkingDirectory -> Script.Host -> Script Int ()
script arguments workingDirectory host =
    Script.printLine "Hello World!"


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
