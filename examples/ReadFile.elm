port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)


script : Script Int ()
script =
    Script.readFile "test.txt"
        |> Script.map String.lines
        |> Script.map (List.filter (not << String.isEmpty))
        |> Script.andThen (Script.forEach (String.toUpper >> Script.print))
        |> Script.onError (.message >> handleError)


handleError : String -> Script Int ()
handleError message =
    Script.print ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.run script requestPort responsePort
