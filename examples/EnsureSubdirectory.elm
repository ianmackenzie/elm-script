module Main exposing (..)

import Common exposing (ensureDirectory, handleError, requestPort, responsePort)
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions exposing (ReadWrite)


script : Script.Context -> Script Int ()
script { fileSystem } =
    let
        subdirectory =
            fileSystem
                |> FileSystem.directory Permissions.readWrite "subdirectory"
    in
    ensureDirectory subdirectory


main : Script.Program
main =
    Script.program script requestPort responsePort
