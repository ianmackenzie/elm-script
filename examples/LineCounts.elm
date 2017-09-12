port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Allowed, FileError, Script)


getLineCount : String -> Script { p | read : Allowed } FileError Int
getLineCount filename =
    Script.readFile filename
        |> Script.map (String.lines >> List.length)


script : List String -> Script { read : Allowed } Int ()
script filenames =
    Script.collect getLineCount filenames
        |> Script.map (List.map2 (,) filenames)
        |> Script.andThen
            (Script.forEach
                (\( filename, lineCount ) ->
                    Script.print
                        (filename ++ ": " ++ toString lineCount ++ " lines")
                )
            )
        |> Script.onError (.message >> handleError)


handleError : String -> Script p Int ()
handleError message =
    Script.do [ Script.print ("ERROR: " ++ message), Script.fail 1 ]


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
