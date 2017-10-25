port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.File as File
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions


script : Script.Context -> Script Int ()
script { fileSystem } =
    let
        inputFile =
            fileSystem
                |> FileSystem.file Permissions.readOnly "test.txt"

        outputFile =
            fileSystem
                |> FileSystem.file Permissions.writeOnly "reversed.txt"
    in
    File.read inputFile
        |> Script.map String.lines
        |> Script.map List.reverse
        |> Script.map (List.filter (not << String.isEmpty))
        |> Script.map (String.join "\n")
        |> Script.andThen (File.writeTo outputFile)
        |> Script.onError (.message >> handleError)


handleError : String -> Script Int a
handleError message =
    Script.print ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
