port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)


script : List String -> Script Int ()
script arguments =
    Script.readFile "test.txt"
        |> Script.map String.lines
        |> Script.map List.reverse
        |> Script.map (List.filter (not << String.isEmpty))
        |> Script.map (String.join "\n")
        |> Script.andThen (Script.writeFile "reversed.txt")
        |> Script.onError (.message >> handleError)


handleError : String -> Script Int ()
handleError message =
    Script.do [ Script.print ("ERROR: " ++ message), Script.fail 1 ]


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
