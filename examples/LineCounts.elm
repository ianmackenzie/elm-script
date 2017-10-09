port module Main exposing (..)

import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (FileError, Script)
import Kintail.Script.File as File exposing (File)
import Kintail.Script.FileSystem as FileSystem
import Kintail.Script.Permissions exposing (Allowed(Allowed))
import Kintail.Script.Process as Process exposing (Process)


getLineCount : File { p | read : Allowed } -> Script FileError Int
getLineCount file =
    Script.readFile file
        |> Script.map (String.lines >> List.length)


script : Process -> Script Int ()
script process =
    let
        fileSystem =
            Process.fileSystem process

        toFile path =
            FileSystem.file { read = Allowed } path fileSystem

        paths =
            Process.arguments process

        files =
            List.map toFile paths
    in
    Script.collect getLineCount files
        |> Script.map (List.map2 (,) paths)
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
