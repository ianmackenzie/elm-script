port module Example exposing (handleError, program)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory, Writable)


handleError : (x -> String) -> x -> Script Int a
handleError toMessage error =
    Script.printLine ("[SCRIPT ERROR] " ++ toMessage error)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


program : (Script.Context -> Script Int ()) -> Script.Program
program script =
    Script.program script requestPort responsePort
