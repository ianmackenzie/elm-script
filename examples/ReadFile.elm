port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Context, Script)
import Kintail.Script.FileSystem as FileSystem
import Kintail.Script.Permissions as Permissions


script : Context -> Script Int ()
script { arguments, fileSystem } =
    case arguments of
        [ path ] ->
            fileSystem
                |> FileSystem.file Permissions.readOnly path
                |> Script.readFile
                |> Script.map String.lines
                |> Script.map (List.filter (not << String.isEmpty))
                |> Script.andThen
                    (Script.forEach (String.toUpper >> Script.print))
                |> Script.onError (.message >> handleError)

        _ ->
            Script.do
                [ Script.print "Please supply the filename of one file to read"
                , Script.fail 1
                ]


handleError : String -> Script p Int ()
handleError message =
    Script.do [ Script.print ("ERROR: " ++ message), Script.fail 1 ]


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
