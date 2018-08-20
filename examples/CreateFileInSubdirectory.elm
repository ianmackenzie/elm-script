module Main exposing (..)

import Common exposing (ensureDirectory, handleError, requestPort, responsePort)
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions


script : Script.Context -> Script Int ()
script { fileSystem } =
    let
        name =
            "subdirectory"

        subdirectory =
            fileSystem
                |> FileSystem.directory Permissions.readWrite name

        file =
            subdirectory |> Directory.file "child.txt"
    in
    ensureDirectory subdirectory
        |> Script.andThen
            (\() ->
                File.writeTo file "dummy contents"
                    |> Script.onError (handleError .message)
            )


main : Script.Program
main =
    Script.program script requestPort responsePort
