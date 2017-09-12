port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)


script arguments =
    List.range 1 10
        |> Script.forEach
            (\n ->
                Script.print
                    (toString n ++ " squared is " ++ toString (n * n))
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.program script requestPort responsePort
