module Main exposing (..)

import Common exposing (handleError, requestPort, responsePort)
import Script exposing (Script)
import Script.Directory as Directory
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions


script : Script.Context -> Script Int ()
script { fileSystem } =
    let
        subdirectory =
            fileSystem
                |> FileSystem.directory Permissions.readWrite "subdirectory"
    in
    Directory.obliterate subdirectory
        |> Script.onError (handleError .message)


main : Script.Program
main =
    Script.program script requestPort responsePort
