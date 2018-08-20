module Main exposing (..)

import Common exposing (handleError, requestPort, responsePort)
import Script exposing (Script)
import Script.File as File
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions


script : Script.Context -> Script Int ()
script { fileSystem } =
    let
        file =
            fileSystem
                |> FileSystem.file Permissions.writeOnly "reversed.txt"
    in
    File.delete file |> Script.onError (handleError .message)


main : Script.Program
main =
    Script.program script requestPort responsePort
