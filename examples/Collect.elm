port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Script)


filenames =
    [ "ReadFile.elm", "WriteFile.elm", "Example.elm" ]


getLineCount : String -> Script String Int
getLineCount =
    Script.readFile >> Script.map (String.lines >> List.length)


script : Script Int ()
script =
    Script.collect getLineCount filenames
        |> Script.map (List.map2 (,) filenames)
        |> Script.andThen
            (Script.forEach
                (\( filename, lineCount ) ->
                    Script.print
                        (filename ++ ": " ++ toString lineCount ++ " lines")
                )
            )
        |> Script.onError handleError


handleError : String -> Script Int ()
handleError message =
    Script.print ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main =
    Script.run script requestPort responsePort
