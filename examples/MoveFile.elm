port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.File as File
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions


script : Script.Context -> Script Int ()
script { fileSystem } =
    let
        sourceFile =
            fileSystem
                |> FileSystem.file Permissions.readWrite "reversed.txt"

        destinationFile =
            fileSystem
                |> FileSystem.file Permissions.writeOnly "reversed-moved.txt"
    in
    File.move sourceFile destinationFile
        |> Script.onError (handleError .message)


handleError : (x -> String) -> x -> Script Int a
handleError toMessage error =
    Script.printLine ("ERROR: " ++ toMessage error)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
