port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Context, FileError, Script)
import Kintail.Script.File as File exposing (File)
import Kintail.Script.FileSystem as FileSystem
import Kintail.Script.Permissions as Permissions exposing (Read, ReadOnly)


getLineCount : File (Read p) -> Script FileError Int
getLineCount file =
    Script.readFile file
        |> Script.map (String.lines >> List.length)


script : Context -> Script Int ()
script { arguments, fileSystem } =
    let
        toFile path =
            FileSystem.file Permissions.readOnly path fileSystem
    in
    Script.collect getLineCount (List.map toFile arguments)
        |> Script.map (List.map2 (,) arguments)
        |> Script.andThen
            (Script.forEach
                (\( filename, lineCount ) ->
                    Script.print
                        (filename ++ ": " ++ toString lineCount ++ " lines")
                )
            )
        |> Script.onError (.message >> handleError)


handleError : String -> Script Int ()
handleError message =
    Script.do [ Script.print ("ERROR: " ++ message), Script.fail 1 ]


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
