port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)


handleError : String -> Script Int ()
handleError message =
    Script.print ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


script : Script Int ()
script =
    Script.readFile "test.txt"
        |> Script.map String.lines
        |> Script.andThen
            (\lines ->
                Script.forEach (List.filter (not << String.isEmpty) lines)
                    (\line -> Script.print (String.toUpper line))
            )
        |> Script.onError handleError


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.run script requestPort responsePort
