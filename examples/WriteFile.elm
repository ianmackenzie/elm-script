port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)


script : Script Never ()
script =
    Script.readFile "test.txt"
        |> Script.map String.lines
        |> Script.map List.reverse
        |> Script.map (List.filter (not << String.isEmpty))
        |> Script.map (String.join "\n")
        |> Script.andThen (Script.writeFile "reversed.txt")
        |> Script.onError Script.print


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.run script requestPort responsePort
