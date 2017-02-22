port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)


script : Script Never ()
script =
    Script.readFile "test.txt"
        |> Script.map String.lines
        |> Script.andThen
            (\lines ->
                Script.forEach (List.filter (not << String.isEmpty) lines)
                    (\line -> Script.print (String.toUpper line))
            )
        |> Script.onError (\message -> Script.print ("ERROR: " ++ message))


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.run script requestPort responsePort
