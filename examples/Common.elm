port module Common exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)


handleError : (x -> String) -> x -> Script Int a
handleError toMessage error =
    Script.printLine ("ERROR: " ++ toMessage error)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg
